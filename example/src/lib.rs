use dumle::{element, Context, div, Vnode};
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

fn render(ctx: Context, switch: bool) -> impl Vnode  {
    div
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
    };

    render(ctx, true);
}
