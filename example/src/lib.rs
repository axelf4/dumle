// #![feature(trace_macros)]

// trace_macros!(true);

use dumle::{html, tags::*, Child, Context, Listener, Text, Vnode};
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

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(module = "/index.js")]
    fn exit_with_live_runtime();
}

fn render(switch: bool) -> impl Vnode {
    html! {
    <div>{"hej"}
    <button style=if switch { "color: red" } else { "color: blue" },>
    {"imma button"}
    </button>
    </div>
    <button click=move || console_log!("pressed!"),>{"press me"}</button>
    }
    /*Child(
        Listener::unattached(button, "click", move || console_log!("hfes")),
        Text("press me"),
    )*/
}

#[wasm_bindgen(start)]
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

    exit_with_live_runtime();
}
