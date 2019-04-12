use dumle::{tags::*, Attribute, ChildCons, Context, Text, Vnode};
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
    (
        (
            ChildCons(div, Text("hej")),
            Attribute {
                name: "style",
                value: if switch { "color: red" } else { "color: blue" },
                node: ChildCons(button, Text("imma button")),
            },
        ),
        ChildCons(button, Text("press me you fuck")),
    )
}

#[wasm_bindgen]
pub fn run() {
    console_error_panic_hook::set_once();

    // Get the document's `<body>`
    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();
    let body = document.body().unwrap();

    let tree = render(false);
    Context::from(body.clone().into()).patch(None, Some(&tree));

    console_log!("Patching everything a second time!");

    let new = render(true);
    Context::from(body.clone().into()).patch(Some(tree), Some(&new));

    console_log!("After second render!");
}
