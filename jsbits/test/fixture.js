// State belongs to the embedded closure, without installing any global API.
let total = 0;
function advance(amount) { total += amount; return total; }
function identity(value) { return value; }
function nullValue() { return null; }
function privateAPI() { return typeof globalThis.advance === 'undefined'; }
