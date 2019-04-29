# dumle

Sweet virtual DOM library for Rust and WebAssembly.

## Example

The following example creates a simple counter that can be incremented:

```rust
use dumle::{hook::UseState, html, Context};
use wasm_bindgen::{prelude::*, throw_str, UnwrapThrowExt};

#[wasm_bindgen(start)]
pub fn run() {
	// Get the document's `<body>`
	let body = web_sys::window()
		.and_then(|window| window.document())
		.and_then(|document| document.body())
		.unwrap_throw();

	// Render a simple virtual node with a counter
	let vnode = UseState::new(|state: &i32, set_state| {
		html! {
			<div>
				{ state.to_string() }
				<button click=move |_| set_state(&|state: &mut i32| *state += 1),>
					{"Increment"}
				</button>
			</div>
		}
	});

	// Patch the real DOM to match the virtual DOM
	Context::from(body.clone().into()).patch(None, Some(&vnode));

	throw_str("SimulateInfiniteLoop") // Exit with live runtime
}
```

## Design

Dumle aims only to be a virtual DOM library, not a full web application framework.
Constructing virtual trees with dumle should add near zero overhead.
To accomplish that, zero-sized types with inlining is used.
For example, the macro invocation

```rust
html! {
	<div>
		<button />
	</div>
	<img />
}
```

gets turned into `(Child(div, button), img)` which is a zero-sized type.
Thus the compiler can inline the whole reconciliation phase, should it want to.
This means that dumle can get by with a simple `macro_rules!` macro instead of
a full blown procedural macro while generating the same code.

While patching the DOM, dumle maintains a cursor pointing at the current DOM node
instead of storing mounted DOM nodes in their corresponding virtual nodes.
This makes supporting functionality otherwise provided by something like
[React Fragments](https://reactjs.org/docs/fragments.html) trivial,
and reduces the memory size of virtual trees at the cost of always having to
traverse the DOM tree (which isn't too expensive).
Upcoming features to the Rust ecosystem such as specialization and host bindings
should improve performance for free.

No attempt is made at diffing similar sub-trees. That means that given

```rust
use dumle::{html, Either};
if switch {
	Either::A(html! {<div>{"One"}</div>})
} else {
	Either::B(html! {<div>{"Two"}</div>})
}
```

toggling `switch` would remove the `<div>` and then re-add the new `<div>`.
This should rarely matter.
