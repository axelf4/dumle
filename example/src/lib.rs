use dumle::{element, Context, TraversalMode};
use dumle_macro::render;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

#[render]
fn render(ctx: Context, switch: bool) {
    element(ctx);
}

#[wasm_bindgen]
pub fn run() {
    // Get the document's `<body>`
    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();
    let body = document.body().unwrap();
    console_log!("{:?}", body);

    let ctx = Context {
        cursor: body.into(),
        mode: TraversalMode::Creating,
    };

    render(ctx, true);
}
