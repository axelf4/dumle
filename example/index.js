// Note that a dynamic `import` statement here is required due to
// webpack/webpack#6615, but in theory `import { greet } from './pkg/hello_world';`
// will work here one day as well!
const rust = import('./pkg/example');

rust
	.then(m => m.run())
	.catch(console.error);

