use quote::{ToTokens, __private::Span};
use syn::*;

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub methods: Vec<Method>,
}

#[derive(Debug)]
pub struct Method {
    pub name: String,
    pub full_path_name: String,
    pub self_param: Option<Param>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
}

impl Method {
    pub fn from_syn(m: &ImplItemMethod, self_type: &TypePath) -> Method {
        let self_ident = self_type.path.get_ident().unwrap();
        let method_ident = &m.sig.ident;
        let extern_ident = Ident::new(
            format!("{}_{}", &self_ident.to_string(), method_ident.to_string()).as_str(),
            m.sig.ident.span(),
        );

        let all_params = m
            .sig
            .inputs
            .iter()
            .filter_map(|a| match a {
                FnArg::Receiver(_) => None,
                FnArg::Typed(t) => Some(Param::from_pattype(t)),
            })
            .collect::<Vec<_>>();

        let self_param = m.sig.receiver().map(|rec| match rec {
            FnArg::Receiver(rec) => Param {
                name: "self".to_string(),
                tpe: Type {
                    name: self_ident.to_string(),
                    mutability: rec.mutability.is_some(),
                    reference: rec.reference.is_some(),
                },
            },
            _ => panic!("Unexpected self param type"),
        });

        let return_tpe = match &m.sig.output {
            ReturnType::Type(_, return_typ) => Some(Type::from_syn(return_typ.as_ref())),
            ReturnType::Default => None,
        };

        Method {
            name: method_ident.to_string(),
            full_path_name: extern_ident.to_string(),
            self_param,
            params: all_params,
            return_type: return_tpe,
        }
    }
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub tpe: Type,
}

impl Param {
    pub fn from_pattype(t: &PatType) -> Param {
        let ident = match t.pat.as_ref() {
            Pat::Ident(ident) => ident.clone(),
            _ => panic!("Unexpected param type"),
        };

        Param {
            name: ident.ident.to_string(),
            tpe: Type::from_syn(t.ty.as_ref()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Type {
    pub name: String,
    pub reference: bool,
    pub mutability: bool,
}

impl Type {
    pub fn from_syn(tpe: &syn::Type) -> Type {
        match tpe {
            syn::Type::Reference(r) => {
                let mut without_ref = Type::from_syn(r.elem.as_ref());
                without_ref.reference = true;
                without_ref.mutability = r.mutability.is_some();
                without_ref
            }
            syn::Type::Path(p) => Type {
                name: p.path.to_token_stream().to_string(),
                reference: false,
                mutability: false,
            },
            _ => panic!(),
        }
    }

    pub fn to_syn(&self) -> syn::Type {
        if self.reference {
            let mut without_ref = self.clone();
            without_ref.reference = false;
            without_ref.mutability = false;

            syn::Type::Reference(TypeReference {
                and_token: syn::token::And(Span::call_site()),
                lifetime: None,
                mutability: if self.mutability {
                    Some(syn::token::Mut(Span::call_site()))
                } else {
                    None
                },
                elem: Box::new(without_ref.to_syn()),
            })
        } else {
            syn::Type::Path(syn::parse_str(self.name.as_str()).unwrap())
        }
    }
}
