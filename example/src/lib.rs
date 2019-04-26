#![feature(impl_trait_in_bindings)]
// #![feature(trace_macros)]

// trace_macros!(true);

use dumle::{hook::UseState, html, Context, Either, KeyedNode, Vnode};
use std::default::Default;
use wasm_bindgen::{prelude::*, throw_str, UnwrapThrowExt};

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

struct State {
    number: i32,
}

impl Default for State {
    fn default() -> Self {
        State { number: 42 }
    }
}

fn render(switch: bool) -> impl Vnode {
    html! {
    {
        UseState::new(|state: &State, set_state| {
            html! {
                <div>
                { state.number.to_string() }
                <button style=if state.number % 2 == 0 { "color: red;" } else { "color: blue;" },
                    click=move |_| set_state(&|state: &mut State| state.number += 1),>
                    {"Click me"}
                </button>
                {
                    if state.number % 2 == 0 {
                        Either::A(html! {<button>{"Even"}</button>})
                    } else {
                        // Either::B(html! {<div>{"Odd"}</div>})
                        Either::B(())
                    }
                }
                {
                    let iter = (0..state.number).into_iter();
                    let mut vec = if state.number % 2 == 0 {
                        iter.collect::<Vec<_>>()
                    } else {
                        iter.rev().collect()
                    };

                    if state.number % 5 == 0 {
                        vec.retain(|&x| x % 2 == 0);
                    }

                    vec.into_iter().map(|i| KeyedNode::of(i, html!{<div>{i.to_string()}</div>})).collect::<Vec<_>>()
                }
                </div>
            }
        })
    }
    <button click=move |_| console_log!("pressed!"),>{"press me"}</button>
    }
}

#[wasm_bindgen(start)]
pub fn run() {
    console_error_panic_hook::set_once();

    // Get the document's `<body>`
    let window = web_sys::window().unwrap_throw();
    let document = window.document().unwrap_throw();
    let body = document.body().unwrap_throw();

    let tree = render(false);
    Context::from(body.clone().into()).patch(None, Some(&tree));

    console_log!("Patching everything a second time!");

    let new = render(true);
    Context::from(body.clone().into()).patch(Some(tree), Some(&new));
    console_log!("After second render!");

    throw_str("SimulateInfiniteLoop") // Exit with live runtime
}
