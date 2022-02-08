use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::*;

use diplomat_core::ast;

fn gen_params_at_boundary(param: &ast::Param, expanded_params: &mut Vec<FnArg>) {
    match &param.ty {
        ast::TypeName::StrReference(_) | ast::TypeName::PrimitiveSlice(..) => {
            let data_type = if let ast::TypeName::PrimitiveSlice(.., prim) = &param.ty {
                ast::TypeName::Primitive(*prim).to_syn().to_token_stream()
            } else {
                quote! { u8 }
            };
            expanded_params.push(FnArg::Typed(PatType {
                attrs: vec![],
                pat: Box::new(Pat::Ident(PatIdent {
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    ident: Ident::new(&format!("{}_diplomat_data", param.name), Span::call_site()),
                    subpat: None,
                })),
                colon_token: syn::token::Colon(Span::call_site()),
                ty: Box::new(
                    parse2({
                        if let ast::TypeName::PrimitiveSlice(_, ast::Mutability::Mutable, _) =
                            &param.ty
                        {
                            quote! { *mut #data_type }
                        } else {
                            quote! { *const #data_type }
                        }
                    })
                    .unwrap(),
                ),
            }));

            expanded_params.push(FnArg::Typed(PatType {
                attrs: vec![],
                pat: Box::new(Pat::Ident(PatIdent {
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    ident: Ident::new(&format!("{}_diplomat_len", param.name), Span::call_site()),
                    subpat: None,
                })),
                colon_token: syn::token::Colon(Span::call_site()),
                ty: Box::new(
                    parse2(quote! {
                        usize
                    })
                    .unwrap(),
                ),
            }));
        }
        o => {
            expanded_params.push(FnArg::Typed(PatType {
                attrs: vec![],
                pat: Box::new(Pat::Ident(PatIdent {
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    ident: Ident::new(param.name.as_str(), Span::call_site()),
                    subpat: None,
                })),
                colon_token: syn::token::Colon(Span::call_site()),
                ty: Box::new(o.to_syn()),
            }));
        }
    }
}

fn gen_params_invocation(param: &ast::Param, expanded_params: &mut Vec<Expr>) {
    match &param.ty {
        ast::TypeName::StrReference(_) | ast::TypeName::PrimitiveSlice(..) => {
            let data_ident =
                Ident::new(&format!("{}_diplomat_data", param.name), Span::call_site());
            let len_ident = Ident::new(&format!("{}_diplomat_len", param.name), Span::call_site());

            let tokens = if let ast::TypeName::PrimitiveSlice(_, mutability, _) = &param.ty {
                match mutability {
                    ast::Mutability::Mutable => quote! {
                        unsafe { core::slice::from_raw_parts_mut(#data_ident, #len_ident) }
                    },
                    ast::Mutability::Immutable => quote! {
                        unsafe { core::slice::from_raw_parts(#data_ident, #len_ident) }
                    },
                }
            } else {
                // TODO(#57): don't just unwrap? or should we assume that the other side gives us a good value?
                quote! {
                    unsafe {
                        core::str::from_utf8(core::slice::from_raw_parts(#data_ident, #len_ident)).unwrap()
                    }
                }
            };
            expanded_params.push(parse2(tokens).unwrap());
        }
        _ => {
            expanded_params.push(Expr::Path(ExprPath {
                attrs: vec![],
                qself: None,
                path: Ident::new(param.name.as_str(), Span::call_site()).into(),
            }));
        }
    }
}

fn gen_custom_type_method(strct: &ast::CustomType, m: &ast::Method) -> Item {
    let self_ident = Ident::new(strct.name().as_str(), Span::call_site());
    let method_ident = Ident::new(m.name.as_str(), Span::call_site());
    let extern_ident = Ident::new(m.full_path_name.as_str(), Span::call_site());

    let mut all_params = vec![];
    m.params.iter().for_each(|p| {
        gen_params_at_boundary(p, &mut all_params);
    });

    let mut all_params_invocation = vec![];
    m.params.iter().for_each(|p| {
        gen_params_invocation(p, &mut all_params_invocation);
    });

    let this_ident = Pat::Ident(PatIdent {
        attrs: vec![],
        by_ref: None,
        mutability: None,
        ident: Ident::new("this", Span::call_site()),
        subpat: None,
    });

    if let Some(self_param) = &m.self_param {
        all_params.insert(
            0,
            FnArg::Typed(PatType {
                attrs: vec![],
                pat: Box::new(this_ident.clone()),
                colon_token: syn::token::Colon(Span::call_site()),
                ty: Box::new(self_param.to_typename().to_syn()),
            }),
        );
    }

    let lifetimes = {
        let lifetime_env = &m.lifetime_env;
        if lifetime_env.is_empty() {
            quote! {}
        } else {
            quote! { <#lifetime_env> }
        }
    };

    let method_invocation = if m.self_param.is_some() {
        quote! { #this_ident.#method_ident }
    } else {
        quote! { #self_ident::#method_ident }
    };

    let return_tokens = if let Some(return_type) = &m.return_type {
        let return_type_syn = return_type.to_syn();
        if let ast::TypeName::Result(..) = return_type {
            quote! { -> Box<#return_type_syn> }
        } else {
            quote! { -> #return_type_syn }
        }
    } else {
        quote! {}
    };

    let invocation_tokens = if let Some(ast::TypeName::Result(..)) = &m.return_type {
        quote! { Box::new(#method_invocation(#(#all_params_invocation),*)) }
    } else {
        quote! { #method_invocation(#(#all_params_invocation),*) }
    };

    Item::Fn(syn::parse_quote! {
        #[no_mangle]
        extern "C" fn #extern_ident#lifetimes(#(#all_params),*) #return_tokens {
            #invocation_tokens
        }
    })
}

struct AttributeInfo {
    repr: bool,
    opaque: bool,
}

impl AttributeInfo {
    fn extract(attrs: &mut Vec<Attribute>) -> Self {
        let mut repr = false;
        let mut opaque = false;
        attrs.retain(|attr| {
            let ident = &attr.path.segments.iter().next().unwrap().ident;
            if ident == "repr" {
                repr = true;
                // don't actually extract repr attrs, just detect them
                return true;
            } else if ident == "diplomat" {
                if attr.path.segments.len() == 2 {
                    let seg = &attr.path.segments.iter().nth(1).unwrap().ident;
                    if seg == "opaque" {
                        opaque = true;
                        return false;
                    } else if seg == "rust_link" {
                        return false;
                    } else {
                        panic!("Only #[diplomat::opaque] and #[diplomat::rust_link] are supported")
                    }
                } else {
                    panic!("#[diplomat::foo] attrs have a single-segment path name")
                }
            }
            true
        });

        Self { repr, opaque }
    }
}

fn gen_bridge(input: ItemMod) -> ItemMod {
    let module = ast::Module::from_syn(&input, true);
    let (brace, mut new_contents) = input.content.unwrap();

    new_contents.iter_mut().for_each(|c| match c {
        Item::Struct(s) => {
            let info = AttributeInfo::extract(&mut s.attrs);
            if info.opaque || !info.repr {
                let repr = if info.opaque {
                    quote!(#[repr(transparent)])
                } else {
                    quote!(#[repr(C)])
                };
                *s = syn::parse_quote! {
                    #repr
                    #s
                }
            }
        }

        Item::Enum(e) => {
            let info = AttributeInfo::extract(&mut e.attrs);
            if info.opaque {
                panic!("#[diplomat::opaque] not allowed on enums")
            }
            *e = syn::parse_quote! {
                #[repr(C)]
                #e
            };
        }

        Item::Impl(i) => {
            for item in &mut i.items {
                if let syn::ImplItem::Method(ref mut m) = *item {
                    let info = AttributeInfo::extract(&mut m.attrs);
                    if info.opaque {
                        panic!("#[diplomat::opaque] not allowed on methods")
                    }
                }
            }
        }
        _ => (),
    });

    let mut results = Vec::new();

    for custom_type in module.declared_types.values() {
        custom_type.methods().iter().for_each(|m| {
            new_contents.push(gen_custom_type_method(custom_type, m));
            if let Some(return_type) = m.return_type.as_ref() {
                collect_results(return_type, &mut results);
            }
        });

        let destroy_ident = Ident::new(
            format!("{}_destroy", custom_type.name()).as_str(),
            Span::call_site(),
        );

        let type_ident = custom_type.name().to_syn();

        let (lifetime_defs, lifetimes) = if let Some(lifetime_env) = custom_type.lifetimes() {
            (
                quote! { <#lifetime_env> },
                lifetime_env.lifetimes_to_tokens(),
            )
        } else {
            (quote! {}, quote! {})
        };

        // for now, body is empty since all we need to do is drop the box
        // TODO(#13): change to take a `*mut` and handle DST boxes appropriately
        new_contents.push(Item::Fn(syn::parse_quote! {
            #[no_mangle]
            extern "C" fn #destroy_ident#lifetime_defs(this: Box<#type_ident#lifetimes>) {}
        }));
    }

    for result in results {
        let (ok, err) = if let ast::TypeName::Result(ok, err) = &result {
            (ok, err)
        } else {
            panic!();
        };

        let result_str = gen_type_name_for_result_destroy(&result);

        let destroy_ident = Ident::new(&format!("{result_str}_destroy"), Span::call_site());

        let result: syn::Type = match (ok.as_ref(), err.as_ref()) {
            (ast::TypeName::Unit, ast::TypeName::Unit) => syn::parse_quote! {
                ::diplomat_runtime::DiplomatResult<(), ()>
            },
            (ast::TypeName::Unit, _) => {
                let err_ident = err.to_syn();
                syn::parse_quote! {
                    ::diplomat_runtime::DiplomatResult<(), #err_ident>
                }
            }
            (_, ast::TypeName::Unit) => {
                let ok_ident = ok.to_syn();
                syn::parse_quote! {
                    ::diplomat_runtime::DiplomatResult<#ok_ident, ()>
                }
            }
            (_, _) => {
                let ok_ident = ok.to_syn();
                let err_ident = err.to_syn();
                syn::parse_quote! {
                    ::diplomat_runtime::DiplomatResult<#ok_ident, #err_ident>
                }
            }
        };

        new_contents.push(Item::Fn(syn::parse_quote! {
            #[no_mangle]
            extern "C" fn #destroy_ident(this: Box<#result>) {}
        }));
    }

    ItemMod {
        attrs: input.attrs,
        vis: input.vis,
        mod_token: input.mod_token,
        ident: input.ident,
        content: Some((brace, new_contents)),
        semi: input.semi,
    }
}

/// Mark a module to be exposed through Diplomat-generated FFI.
#[proc_macro_attribute]
pub fn bridge(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let expanded = gen_bridge(parse_macro_input!(input));
    proc_macro::TokenStream::from(expanded.to_token_stream())
}

lazy_static::lazy_static! {
    static ref RESULTS_SETS: std::sync::Mutex<std::collections::HashSet<String>> = std::sync::Mutex::new(std::collections::HashSet::new());
}

fn collect_results<'ast>(typ: &'ast ast::TypeName, results: &mut Vec<&'ast ast::TypeName>) {
    match typ {
        ast::TypeName::Box(underlying) => {
            collect_results(underlying, results);
        }
        ast::TypeName::Reference(_lt, _mut, underlying) => {
            collect_results(underlying, results);
        }
        ast::TypeName::Option(underlying) => {
            collect_results(underlying, results);
        }
        ast::TypeName::Result(ok, err) => {
            let mut results_set = RESULTS_SETS.lock().unwrap();
            let typ_str = typ.to_string();
            if !results_set.contains(&typ_str) {
                results_set.insert(typ_str);
                results.push(typ);
                collect_results(ok, results);
                collect_results(err, results);
            }
        }
        ast::TypeName::Unit
        | ast::TypeName::Writeable
        | ast::TypeName::StrReference(..)
        | ast::TypeName::PrimitiveSlice(..)
        | ast::TypeName::Named(_)
        | ast::TypeName::Primitive(_) => {}
    }
}

fn gen_type_name_for_result_destroy(typ: &ast::TypeName) -> String {
    match typ {
        ast::TypeName::Named(_) => typ.to_string(),
        ast::TypeName::Box(underlying) => format!(
            "box_{}",
            gen_type_name_for_result_destroy(underlying.as_ref())
        ),
        ast::TypeName::Reference(_lt, _mutable, underlying) => {
            format!(
                "ref_{}",
                gen_type_name_for_result_destroy(underlying.as_ref())
            )
        }
        ast::TypeName::Option(underlying) => format!(
            "opt_{}",
            gen_type_name_for_result_destroy(underlying.as_ref())
        ),
        ast::TypeName::Result(ok, err) => {
            format!(
                "result_{}_{}",
                gen_type_name_for_result_destroy(ok.as_ref()),
                gen_type_name_for_result_destroy(err.as_ref())
            )
        }
        ast::TypeName::Primitive(prim) => prim.to_string(),
        ast::TypeName::Writeable => "DiplomatWriteable".to_owned(),
        ast::TypeName::StrReference(_mut) => "str".to_owned(),
        ast::TypeName::PrimitiveSlice(_lt, _mut, prim) => {
            format!("{}_slice", prim)
        }
        ast::TypeName::Unit => "unit".to_owned(),
    }
}
#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::{Read, Write};
    use std::process::Command;

    use quote::ToTokens;
    use syn::parse_quote;
    use tempfile::tempdir;

    use super::gen_bridge;

    fn rustfmt_code(code: &str) -> String {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("temp.rs");
        let mut file = File::create(file_path.clone()).unwrap();

        writeln!(file, "{}", code).unwrap();
        drop(file);

        Command::new("rustfmt")
            .arg(file_path.to_str().unwrap())
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        let mut file = File::open(file_path).unwrap();
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();
        drop(file);
        dir.close().unwrap();
        data
    }

    #[test]
    fn method_taking_str() {
        insta::assert_display_snapshot!(rustfmt_code(
            &gen_bridge(parse_quote! {
                mod ffi {
                    struct Foo {}

                    impl Foo {
                        pub fn from_str(s: &str) {
                            unimplemented!()
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn method_taking_slice() {
        insta::assert_display_snapshot!(rustfmt_code(
            &gen_bridge(parse_quote! {
                mod ffi {
                    struct Foo {}

                    impl Foo {
                        pub fn from_slice(s: &[f64]) {
                            unimplemented!()
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn method_taking_mutable_slice() {
        insta::assert_display_snapshot!(rustfmt_code(
            &gen_bridge(parse_quote! {
                mod ffi {
                    struct Foo {}

                    impl Foo {
                        pub fn fill_slice(s: &mut [f64]) {
                            unimplemented!()
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn mod_with_enum() {
        insta::assert_display_snapshot!(rustfmt_code(
            &gen_bridge(parse_quote! {
                mod ffi {
                    enum Abc {
                        A,
                        B = 123,
                    }

                    impl Abc {
                        pub fn do_something(&self) {
                            unimplemented!()
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn mod_with_writeable_result() {
        insta::assert_display_snapshot!(rustfmt_code(
            &gen_bridge(parse_quote! {
                mod ffi {
                    struct Foo {}

                    impl Foo {
                        pub fn to_string(&self, to: &mut DiplomatWriteable) -> DiplomatResult<(), ()> {
                            unimplemented!()
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn multilevel_borrows() {
        insta::assert_display_snapshot!(rustfmt_code(
            &gen_bridge(parse_quote! {
                mod ffi {
                    #[diplomat::opaque]
                    struct Foo<'a>(&'a str);

                    #[diplomat::opaque]
                    struct Bar<'b, 'a: 'b>(&'b Foo<'a>);

                    struct Baz<'x, 'y> {
                        foo: &'y Foo<'x>,
                    }

                    impl<'a> Foo<'a> {
                        pub fn new(x: &'a str) -> Box<Foo<'a>> {
                            unimplemented!()
                        }

                        pub fn get_bar<'b>(&'b self) -> Box<Bar<'b, 'a>> {
                            unimplemented!()
                        }

                        pub fn get_baz<'b>(&'b self) -> Baz<'b, 'a> {
                            Bax { foo: self }
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }

    #[test]
    fn self_params() {
        insta::assert_display_snapshot!(rustfmt_code(
            &gen_bridge(parse_quote! {
                mod ffi {
                    #[diplomat::opaque]
                    struct RefList<'a> {
                        data: &'a i32,
                        next: Option<Box<Self>>,
                    }

                    impl<'b> RefList<'b> {
                        pub fn extend(&mut self, other: &Self) -> Self {
                            unimplemented!()
                        }
                    }
                }
            })
            .to_token_stream()
            .to_string()
        ));
    }
}
