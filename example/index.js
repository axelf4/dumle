export function exit_with_live_runtime() {
	throw "SimulateInfiniteLoop";
}

import('./pkg/example').catch(e => {
	if (e !== "SimulateInfiniteLoop") console.error(e);
});
