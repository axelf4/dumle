use dumle::{patch, tags::*, Context, Vnode};
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

fn render(switch: bool) -> impl Vnode {
    ((div, button), button)
}

#[wasm_bindgen]
pub fn run() {
    // Get the document's `<body>`
    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();
    let body = document.body().unwrap();
    console_log!("{:?}", body);

    let mut ctx = Context::from(body.into());

    let tree = render(true);

    patch(&mut ctx, None, Some(&tree));

    let new = render(true);

    patch(&mut ctx, Some(tree), Some(&new));
}
