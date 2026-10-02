// Exercise both browser implementations without playing sound or fetching files.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const root = path.resolve(__dirname, '..');
const jsbits = fs.readFileSync(path.join(root, 'jsbits/sound.js'), 'utf8');
const wasm = fs.readFileSync(path.join(root, 'src/Halogen/Sound/Browser.hs'), 'utf8');
const inline = wasm.match(/foreign import javascript unsafe "(const a = new Audio.*?)" js_start/)[1];
const parameters = ['url', 'volume', 'looping', 'held', 'offset', 'heard', 'ended'];
const body = inline.replace(/\$(\d+)/g, (_, n) => parameters[Number(n) - 1]);

for (const [backend, source] of [
  ['javascript', jsbits],
  ['wasm', `function halogen_sound_start(${parameters.join(', ')}) { ${body} }`],
]) {
  const voices = [];
  class Audio {
    constructor() { this.readyState = 0; this.duration = NaN; this.currentTime = 0; this.plays = []; voices.push(this); }
    play() { this.plays.push(this.currentTime); this.onplaying(); return Promise.resolve(); }
    pause() {}
    metadata(duration = 60) { this.duration = duration; this.readyState = 1; if (this.onloadedmetadata) this.onloadedmetadata(); }
  }
  const context = { Audio, addEventListener() {}, removeEventListener() {} };
  vm.createContext(context);
  vm.runInContext(source, context);
  const start = (held, offset) => context.halogen_sound_start('blob:track', 0.5, false, held, offset, () => {}, () => {});

  const playing = start(false, 23.5);
  assert.deepEqual(playing.plays, [], backend + ': wait for metadata before playback');
  playing.metadata();
  assert.deepEqual(playing.plays, [23.5], backend + ': seek before the first play');
  playing.currentTime = 30;
  playing.__halogenGo();
  assert.deepEqual(playing.plays, [23.5, 30], backend + ': never seek again on resume');

  const paused = start(true, 17);
  paused.metadata();
  assert.equal(paused.currentTime, 17, backend + ': preserve position while held');
  assert.deepEqual(paused.plays, [], backend + ': held voice is never heard');
  paused.__halogenPaused = false;
  paused.__halogenGo();
  assert.deepEqual(paused.plays, [17]);

  const stopped = start(false, 10);
  stopped.__halogenStopped = true;
  stopped.metadata();
  assert.deepEqual(stopped.plays, [], backend + ': late metadata cannot revive a stopped voice');

  const pastEnd = start(false, 100);
  pastEnd.metadata();
  assert.ok(pastEnd.currentTime > 59 && pastEnd.currentTime < 60, backend + ': clamp to duration');

  for (const at of [-1, NaN, Infinity]) {
    const invalid = start(false, at);
    invalid.metadata();
    assert.equal(invalid.currentTime, 0, backend + ': invalid offset starts at zero');
  }
  console.log(backend + ': browser offset checks passed');
}
