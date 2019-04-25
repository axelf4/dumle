export function exit_with_live_runtime() {
	throw "SimulateInfiniteLoop";
}

import('./pkg/example').catch(e => {
	if (e.message !== "SimulateInfiniteLoop") console.error(e);
});
