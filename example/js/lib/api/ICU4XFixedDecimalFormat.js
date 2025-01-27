import wasm from "./diplomat-wasm.mjs"
import * as diplomatRuntime from "./diplomat-runtime.js"
import { ICU4XFixedDecimalFormatResult } from "./ICU4XFixedDecimalFormatResult.js"
import { ICU4XFixedDecimalGroupingStrategy_js_to_rust, ICU4XFixedDecimalGroupingStrategy_rust_to_js } from "./ICU4XFixedDecimalGroupingStrategy.js"
import { ICU4XFixedDecimalSignDisplay_js_to_rust, ICU4XFixedDecimalSignDisplay_rust_to_js } from "./ICU4XFixedDecimalSignDisplay.js"

const ICU4XFixedDecimalFormat_box_destroy_registry = new FinalizationRegistry(underlying => {
  wasm.ICU4XFixedDecimalFormat_destroy(underlying);
});

export class ICU4XFixedDecimalFormat {
  #lifetimeEdges = [];
  constructor(underlying, owned, edges) {
    this.underlying = underlying;
    this.#lifetimeEdges.push(...edges);
    if (owned) {
      ICU4XFixedDecimalFormat_box_destroy_registry.register(this, underlying);
    }
  }

  static try_new(arg_locale, arg_provider, arg_options) {
    const field_grouping_strategy_arg_options = arg_options["grouping_strategy"];
    const field_sign_display_arg_options = arg_options["sign_display"];
    return (() => {
      const diplomat_receive_buffer = wasm.diplomat_alloc(5, 4);
      wasm.ICU4XFixedDecimalFormat_try_new(diplomat_receive_buffer, arg_locale.underlying, arg_provider.underlying, ICU4XFixedDecimalGroupingStrategy_js_to_rust[field_grouping_strategy_arg_options], ICU4XFixedDecimalSignDisplay_js_to_rust[field_sign_display_arg_options]);
      const out = new ICU4XFixedDecimalFormatResult(diplomat_receive_buffer);
      wasm.diplomat_free(diplomat_receive_buffer, 5, 4);
      return out;
    })();
  }

  format_write(arg_value) {
    return diplomatRuntime.withWriteable(wasm, (writeable) => {
      return wasm.ICU4XFixedDecimalFormat_format_write(this.underlying, arg_value.underlying, writeable);
    });
  }
}
