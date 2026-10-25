function halogen_test_has_document() {
  return typeof document !== 'undefined';
}

function halogen_test_test_args() {
  return globalThis.__halogenTestArgs ?? null;
}

function halogen_test_remove_leftovers() {
  document.querySelectorAll('.halogen-test-root').forEach((root) => root.remove());
}

function halogen_test_test_done(failures) {
  return globalThis.__halogenTest?.done?.(failures);
}

function halogen_test_create_container() {
  return halogen_test_create_root_in(document.body);
}

// A page's container inside an element the suite was told to run in
// (runBrowserTestsIn), which the page has for something else besides.
function halogen_test_create_root_in(parent) {
  return parent.appendChild(Object.assign(document.createElement('div'), {className: 'halogen-test-root'}));
}

function halogen_test_create_container_in(parent) {
  return parent.appendChild(document.createElement('div'));
}

function halogen_test_set_inner_html(element, html) {
  element.innerHTML = html;
}

function halogen_test_remove(element) {
  return element.remove();
}

function halogen_test_query_selector(scope, selector) {
  return scope.querySelector(selector);
}

function halogen_test_query_selector_all(scope, selector) {
  return Array.from(scope.querySelectorAll(selector));
}

// The runner's actions, each returning the promise of its end, which
// Test.Hspec.Halogen.Internal.Page awaits. A rejection (an action the runner
// could not perform) is handled here as well as returned, so that one whose
// wait was cut short is not reported as unhandled, which the runner counts as
// an error in the page; whoever awaits the promise still gets it.
//
// Under the runner (globalThis.__halogenTest, which runner.mjs and open.mjs
// install) Playwright performs them as trusted input. A page without it, one
// opened by hand or one with no Playwright behind it at all (the tutorial,
// which runs a suite in the GHC it has in the page), gets the in-page mode
// below instead: the same actions as synthetic events, dispatched by the
// page itself. They are untrusted (isTrusted is false), and only what the
// page's own listeners see is reproduced, not what the browser does by
// itself: see the README's "Without the runner".
function halogen_test_quiet(promise) {
  promise.catch(() => {});
  return promise;
}

function halogen_test_act(action, element, text) {
  return halogen_test_quiet(halogen_test_act_now(action, element, text));
}

function halogen_test_drag(source, target) {
  return halogen_test_quiet(halogen_test_drag_now(source, target));
}

function halogen_test_press(key) {
  return halogen_test_quiet(halogen_test_press_now(key));
}

async function halogen_test_act_now(action, element, text) {
  const bridge = globalThis.__halogenTest;
  if (!bridge?.act) {
    await halogen_test_synthetic_act(action, element, text);
    return;
  }
  const target = 'halogen-test-' + (globalThis.__halogenTestTargets = (globalThis.__halogenTestTargets ?? 0) + 1);
  element.setAttribute('data-halogen-test-target', target);
  try {
    await bridge.act(action, '[data-halogen-test-target="' + target + '"]', text);
  } finally {
    element.removeAttribute('data-halogen-test-target');
  }
}

async function halogen_test_drag_now(source, target) {
  const bridge = globalThis.__halogenTest;
  if (!bridge?.drag) {
    await halogen_test_synthetic_drag(source, target);
    return;
  }
  const counter = () => 'halogen-test-' + (globalThis.__halogenTestTargets = (globalThis.__halogenTestTargets ?? 0) + 1);
  const [from, to] = [counter(), counter()];
  source.setAttribute('data-halogen-test-target', from);
  target.setAttribute('data-halogen-test-target', to);
  try {
    await bridge.drag('[data-halogen-test-target="' + from + '"]', '[data-halogen-test-target="' + to + '"]');
  } finally {
    source.removeAttribute('data-halogen-test-target');
    target.removeAttribute('data-halogen-test-target');
  }
}

async function halogen_test_press_now(key) {
  const bridge = globalThis.__halogenTest;
  if (bridge?.press) {
    await bridge.press(key);
  } else {
    await halogen_test_synthetic_press(key);
  }
}

// The in-page mode. Each action first waits for a task of its own, so that
// its events are dispatched as a user's arrive, from the page's event loop
// with no Haskell under them on the stack, and not from inside the foreign
// call that asked for it.
function halogen_test_next_task() {
  return new Promise((resolve) => setTimeout(resolve, 0));
}

// Whether an element is visible as Playwright has it: it has a box, and
// neither it nor what holds it is display: none, content-visibility: hidden
// or visibility: hidden. Opacity is not looked at: Playwright counts an
// element of opacity 0 as visible.
function halogen_test_visible(element) {
  return element.checkVisibility({visibilityProperty: true});
}

// Playwright's actionability checks, as far as a page can make them: the
// element is in the document, visible and not disabled, and for an action
// that edits it (editable), not read-only, or becomes so within five seconds
// (the runner's timeout for an action), else the action fails as it would
// under the runner, with Playwright's words for it. Whether something
// covers it is not checked.
async function halogen_test_actionable(element, what, editable) {
  const deadline = Date.now() + 5000;
  for (;;) {
    const problem = !element.isConnected
      ? 'is not in the page'
      : !halogen_test_visible(element)
        ? 'is not visible'
        : element.matches(':disabled')
          ? 'is disabled'
          : editable && !element.isContentEditable && element.readOnly
            ? 'is not editable'
            : null;
    if (!problem) return;
    if (Date.now() > deadline) {
      throw Object.assign(new Error(what + ': Timeout 5000ms exceeded: the element ' + problem + ': ' + element.outerHTML.slice(0, 200)), {name: 'TimeoutError'});
    }
    await new Promise((resolve) => setTimeout(resolve, 50));
  }
}

function halogen_test_mouse_at(element) {
  const box = element.getBoundingClientRect();
  return {bubbles: true, cancelable: true, composed: true, view: window, button: 0, clientX: box.left + box.width / 2, clientY: box.top + box.height / 2};
}

const halogen_test_pointer = {isPrimary: true, pointerId: 1, pointerType: 'mouse'};

// What a press of the mouse focuses: the element, or the nearest focusable
// one around it, as a browser's would.
function halogen_test_focus_target(element) {
  return element.closest('a[href], button, input, select, textarea, summary, [tabindex], [contenteditable=""], [contenteditable="true"]');
}

// A mouse click: the pointer and mouse events a user's would send, the focus
// a press gives (unless mousedown's default was prevented: to the element or
// the focusable one around it, or away from whatever had it when there is
// none, as a press on plain text takes the focus away), then click(),
// which dispatches the click with its default action (a checkbox toggles, a
// link is followed, a form's button submits it).
async function halogen_test_synthetic_click(element) {
  const at = halogen_test_mouse_at(element);
  element.dispatchEvent(new PointerEvent('pointerdown', {...at, ...halogen_test_pointer, buttons: 1}));
  if (element.dispatchEvent(new MouseEvent('mousedown', {...at, buttons: 1}))) {
    const focusable = halogen_test_focus_target(element);
    if (focusable) focusable.focus();
    else document.activeElement?.blur?.();
  }
  element.dispatchEvent(new PointerEvent('pointerup', {...at, ...halogen_test_pointer}));
  element.dispatchEvent(new MouseEvent('mouseup', at));
  element.click();
}

// Whether an element holds text that typing edits.
function halogen_test_is_text_field(element) {
  return element instanceof HTMLTextAreaElement ||
    (element instanceof HTMLInputElement && !['button', 'checkbox', 'color', 'file', 'hidden', 'image', 'radio', 'range', 'reset', 'submit'].includes(element.type));
}

// The text typed so far into an input whose value sanitises what it is
// given (number, and the others with no selection, such as email), with the
// value it gave. A browser keeps what the user typed apart from the value:
// typing -42 into a number field, its value is "" after the -, as the input
// events say, but the - is still there for the 4 and the 2. Setting the
// value a prefix at a time would lose it, so the text is kept here, and
// each keystroke sets the value from it. It holds only while the value is
// still the one it gave: a render or a script that changed the value since
// starts the text afresh from it, as do clear and a fresh focus.
const halogen_test_raw = new WeakMap();

// HTML's valid floating-point number, which a number field's value must be.
const halogen_test_number = /^-?(\d+(\.\d+)?|\.\d+)([eE][-+]?\d+)?$/;

// The text an edit applies to: the input's value, or for one that
// sanitises, the text typed into it so far.
function halogen_test_text(element) {
  const kept = halogen_test_raw.get(element);
  return kept && kept.value === element.value ? kept.raw : element.value;
}

// Edit a text field as typing would, with the beforeinput and input events
// it sends: replace the selection (from start to end, the caret when they
// are one) with text. An input type with no selection (email, number) is
// edited at the end of what has been typed into it, unless start and end
// say where in that text. A contenteditable element is edited at its
// selection by the browser's own editing commands, which send its input
// event; start and end are not used for it.
//
// As a browser does, it edits nothing in a read-only field (and sends no
// beforeinput there), inserts no more than the field's maxlength allows
// (deleting is not limited), and leaves the field as it was when the
// beforeinput event's default is prevented, which is how a component turns
// an edit down before it happens.
function halogen_test_edit(element, text, inputType, start, end) {
  if (!element.isContentEditable && element.readOnly) return;
  const value = element.isContentEditable ? '' : halogen_test_text(element);
  const [from, to] = start == null ? [value.length, value.length] : [start, end];
  if (text !== '' && !element.isContentEditable && element.maxLength >= 0) {
    // maxlength counts UTF-16 code units, but what does not fit is left
    // out a whole character at a time, as Chromium does: an emoji (two
    // units) does not go into a field with room for one, not even half.
    const room = Math.max(0, element.maxLength - (value.length - (to - from)));
    text = text.slice(0, halogen_test_graphemes(text).filter((b) => b <= room).pop() ?? 0);
    if (text === '') return;
  }
  const data = inputType === 'insertText' ? text : null;
  const init = {inputType, data, bubbles: true, composed: true};
  if (!element.dispatchEvent(new InputEvent('beforeinput', {...init, cancelable: true}))) return;
  if (element.isContentEditable) {
    document.execCommand(inputType === 'insertText' ? 'insertText' : inputType === 'deleteContentForward' ? 'forwardDelete' : 'delete', false, text);
    return;
  }
  const edited = value.slice(0, from) + text + value.slice(to);
  // What is not a number yet (-, 1.) leaves a number field's value empty,
  // as it does a user's; assigned, it would also be logged as a warning.
  element.value = element.type !== 'number' || halogen_test_number.test(edited) ? edited : '';
  // An input with no selection keeps the text typed into it, deletions
  // included, whether or not its sanitised value changed.
  if (halogen_test_selection(element)[0] == null) {
    halogen_test_raw.set(element, {raw: edited, value: element.value});
  }
  try {
    element.setSelectionRange(from + text.length, from + text.length);
  } catch {
    // No selection in this type of input.
  }
  element.dispatchEvent(new InputEvent('input', init));
}

function halogen_test_selection(element) {
  try {
    return element.selectionStart == null ? [null, null] : [element.selectionStart, element.selectionEnd];
  } catch {
    return [null, null];
  }
}

// One key's keydown, keypress and keyup, with the modifiers held, and what
// pressing it does when keydown's default is not prevented: a character goes
// into a text field, Backspace and Delete take one out, Enter clicks a button
// or a link, submits the form of a text input and starts a line in a text
// area, Space clicks a button or a checkbox, and Tab moves the focus. A
// keypress whose default is prevented stops the character and Enter's
// action, as it does in a browser; a read-only field takes the key's events
// but is not edited (halogen_test_edit).
async function halogen_test_key(key, modifiers) {
  const target = document.activeElement ?? document.body;
  const init = {key, bubbles: true, cancelable: true, composed: true, view: window, ...modifiers};
  const character = [...key].length === 1;
  const chord = modifiers.ctrlKey || modifiers.altKey || modifiers.metaKey;
  // A keydown whose default was prevented does nothing more: no character,
  // and no activation of a button on the keyup either.
  const pressed = target.dispatchEvent(new KeyboardEvent('keydown', init));
  if (pressed) {
    const typing = halogen_test_is_text_field(target) || target.isContentEditable;
    const [start, end] = typing ? halogen_test_selection(target) : [null, null];
    // True when no keypress is sent, or its default was not prevented.
    const typed = (character || key === 'Enter') && !chord
      ? target.dispatchEvent(new KeyboardEvent('keypress', {...init, charCode: character ? key.codePointAt(0) : 13}))
      : true;
    if (!typed) {
      // Neither the character nor Enter's action.
    } else if (character && !chord && typing) {
      halogen_test_edit(target, key, 'insertText', start, end);
    } else if ((key === 'Backspace' || key === 'Delete') && typing) {
      const forward = key === 'Delete';
      const inputType = forward ? 'deleteContentForward' : 'deleteContentBackward';
      if (target.isContentEditable) {
        halogen_test_edit(target, '', inputType, null, null);
      } else {
        const text = halogen_test_text(target);
        const length = text.length;
        const [from, to] = start == null
          ? (forward ? [length, length] : [halogen_test_grapheme_before(text, length), length])
          : start === end
            ? (forward ? [start, halogen_test_grapheme_after(text, end)] : [halogen_test_grapheme_before(text, start), end])
            : [start, end];
        halogen_test_edit(target, '', inputType, from, to);
      }
    } else if (key === 'Enter' && target instanceof HTMLTextAreaElement) {
      halogen_test_edit(target, '\n', 'insertLineBreak', start, end);
    } else if (key === 'Enter' && (target instanceof HTMLButtonElement || target instanceof HTMLAnchorElement || (target instanceof HTMLInputElement && ['button', 'submit', 'reset'].includes(target.type)))) {
      target.click();
    } else if (key === 'Enter' && target instanceof HTMLInputElement && target.form) {
      halogen_test_implicit_submit(target);
    } else if (key === 'Tab') {
      halogen_test_tab(target, modifiers.shiftKey);
    }
  }
  const up = document.activeElement ?? document.body;
  if (up.dispatchEvent(new KeyboardEvent('keyup', init)) && pressed && key === ' ' && up === target &&
      (target instanceof HTMLButtonElement || (target instanceof HTMLInputElement && ['button', 'checkbox', 'radio', 'submit', 'reset'].includes(target.type)))) {
    target.click();
  }
}

// Where Backspace and Delete stop in a field's text: a whole character as
// the reader sees it (a grapheme cluster, such as an emoji, which is two
// UTF-16 code units or more), never half of one. Without Intl.Segmenter,
// by code point, which at least never leaves half a surrogate pair. (The
// text kept for a number field is ASCII, where the two agree.)
function halogen_test_graphemes(text) {
  if (typeof Intl !== 'undefined' && Intl.Segmenter) {
    return Array.from(new Intl.Segmenter(undefined, {granularity: 'grapheme'}).segment(text), (s) => s.index).concat(text.length);
  }
  const boundaries = [0];
  for (let i = 0; i < text.length; ) {
    i += text.codePointAt(i) > 0xffff ? 2 : 1;
    boundaries.push(i);
  }
  return boundaries;
}

function halogen_test_grapheme_before(text, at) {
  return halogen_test_graphemes(text).filter((b) => b < at).pop() ?? 0;
}

function halogen_test_grapheme_after(text, at) {
  return halogen_test_graphemes(text).find((b) => b > at) ?? text.length;
}

// The input types Enter submits a form from, and of which a form with no
// submit button may have one only for Enter to submit it (HTML's fields
// that block implicit submission).
const halogen_test_blocking = ['text', 'search', 'url', 'tel', 'email', 'password', 'date', 'month', 'week', 'time', 'datetime-local', 'number'];

// Enter in a form's field, as HTML's implicit submission has it: the form's
// default button, its first submit button in tree order, is activated, so
// its click handlers run and may prevent the submission, and nothing
// happens if it is disabled. A form with no submit button is submitted
// only when it has a single field that blocks implicit submission.
function halogen_test_implicit_submit(input) {
  const form = input.form;
  if (!halogen_test_blocking.includes(input.type)) return;
  // form.elements leaves out image buttons, so the document is searched.
  const button = Array.from(document.querySelectorAll('button, input[type=submit], input[type=image]'))
    .find((element) => element.form === form && (element.type === 'submit' || element.type === 'image'));
  if (button) {
    if (!button.matches(':disabled')) button.click();
    return;
  }
  const fields = Array.from(form.elements).filter((element) => element instanceof HTMLInputElement && halogen_test_blocking.includes(element.type));
  if (fields.length === 1) form.requestSubmit();
}

// Move the focus to the next (or previous) element Tab reaches, in the
// order a browser goes in: the elements with a positive tabindex first, by
// it (in document order for the same one), then those with tabindex 0 or
// none, in document order. Disabled, hidden and tabindex="-1" elements are
// left out.
function halogen_test_tab(from, backwards) {
  const reachable = Array.from(document.querySelectorAll('a[href], button, input, select, textarea, summary, [tabindex], [contenteditable=""], [contenteditable="true"]'))
    .filter((element) => element.tabIndex >= 0 && !element.matches(':disabled') && halogen_test_visible(element))
    .map((element, order) => ({element, order}))
    .sort((a, b) => (a.element.tabIndex || Infinity) - (b.element.tabIndex || Infinity) || a.order - b.order)
    .map(({element}) => element);
  const at = reachable.indexOf(from);
  const next = backwards ? (at <= 0 ? reachable.length - 1 : at - 1) : at + 1;
  reachable[next % reachable.length]?.focus();
}

const halogen_test_modifier_flags = {Shift: 'shiftKey', Control: 'ctrlKey', Alt: 'altKey', Meta: 'metaKey'};

// A key or a chord in Playwright's terms: Enter, a, Shift+Tab,
// ControlOrMeta+a. The modifiers' own keydown and keyup are sent around it.
async function halogen_test_synthetic_press(chord) {
  await halogen_test_next_task();
  const parts = chord.split('+');
  // "Shift++" is Shift and the plus key.
  if (parts.length > 1 && parts[parts.length - 1] === '') parts.splice(-2, 2, '+');
  // Playwright's names for keys by their code: Space, KeyA, Digit1.
  const named = parts.pop();
  const shifted = parts.includes('Shift');
  const key = named === 'Space'
    ? ' '
    : /^Key[A-Z]$/.test(named)
      ? (shifted ? named[3] : named[3].toLowerCase())
      : /^Digit[0-9]$/.test(named)
        ? named[5]
        : named;
  const names = parts.map((name) => (name === 'ControlOrMeta' ? (/Mac/.test(navigator.platform) ? 'Meta' : 'Control') : name));
  const modifiers = {};
  const target = () => document.activeElement ?? document.body;
  for (const name of names) {
    modifiers[halogen_test_modifier_flags[name]] = true;
    target().dispatchEvent(new KeyboardEvent('keydown', {key: name, bubbles: true, cancelable: true, composed: true, ...modifiers}));
  }
  await halogen_test_key(key, modifiers);
  for (const name of names.reverse()) {
    delete modifiers[halogen_test_modifier_flags[name]];
    target().dispatchEvent(new KeyboardEvent('keyup', {key: name, bubbles: true, cancelable: true, composed: true, ...modifiers}));
  }
}

async function halogen_test_synthetic_act(action, element, text) {
  await halogen_test_next_task();
  if (action === 'click') {
    await halogen_test_actionable(element, 'click');
    await halogen_test_synthetic_click(element);
  } else if (action === 'type') {
    // As Playwright's pressSequentially: focus the element, then a key at a
    // time, after what it holds.
    await halogen_test_actionable(element, 'type');
    // Focused anew, an input starts from its value, not from what was typed
    // into it before.
    if (document.activeElement !== element) halogen_test_raw.delete(element);
    element.focus();
    if (halogen_test_is_text_field(element)) {
      try {
        element.setSelectionRange(element.value.length, element.value.length);
      } catch {
        // No selection in this type of input: it is edited at its end.
      }
    } else if (element.isContentEditable) {
      // The caret at the end of what it holds.
      const range = document.createRange();
      range.selectNodeContents(element);
      range.collapse(false);
      const selection = getSelection();
      selection.removeAllRanges();
      selection.addRange(range);
    }
    for (const character of text) await halogen_test_key(character, {});
  } else if (action === 'clear') {
    // As Playwright's clear: only a text field or an element edited in
    // place, once it is editable; then focus, select everything, delete it.
    if (!halogen_test_is_text_field(element) && !element.isContentEditable) {
      throw new Error('clear: the element is not an <input>, <textarea> or [contenteditable] element: ' + element.outerHTML.slice(0, 200));
    }
    await halogen_test_actionable(element, 'clear', true);
    element.focus();
    if (halogen_test_is_text_field(element)) {
      halogen_test_raw.delete(element);
      halogen_test_edit(element, '', 'deleteContentBackward', 0, element.value.length);
    } else if (element.isContentEditable) {
      document.execCommand('selectAll');
      halogen_test_edit(element, '', 'deleteContentBackward', null, null);
    }
  } else {
    throw new Error('unknown action ' + action);
  }
}

// Pointer events for a drag the page follows itself, then HTML5 drag and
// drop's, sharing one DataTransfer as a browser's would.
async function halogen_test_synthetic_drag(source, target) {
  await halogen_test_next_task();
  await halogen_test_actionable(source, 'drag');
  await halogen_test_actionable(target, 'drag');
  const from = halogen_test_mouse_at(source);
  const to = halogen_test_mouse_at(target);
  source.dispatchEvent(new PointerEvent('pointerdown', {...from, ...halogen_test_pointer, buttons: 1}));
  target.dispatchEvent(new PointerEvent('pointermove', {...to, ...halogen_test_pointer, buttons: 1}));
  target.dispatchEvent(new PointerEvent('pointerup', {...to, ...halogen_test_pointer}));
  const dataTransfer = new DataTransfer();
  source.dispatchEvent(new DragEvent('dragstart', {...from, dataTransfer}));
  target.dispatchEvent(new DragEvent('dragenter', {...to, dataTransfer}));
  target.dispatchEvent(new DragEvent('dragover', {...to, dataTransfer}));
  target.dispatchEvent(new DragEvent('drop', {...to, dataTransfer}));
  source.dispatchEvent(new DragEvent('dragend', {...to, dataTransfer}));
}

async function halogen_test_settle() {
  await new Promise((resolve) => globalThis.scheduler
    ? scheduler.postTask(resolve, {priority: 'background'})
    : setTimeout(resolve, 0));
}

function halogen_test_focus(element) {
  return element.focus();
}

function halogen_test_blur(element) {
  return element.blur();
}

function halogen_test_text_content(element) {
  return element.textContent ?? '';
}

function halogen_test_get_property(element, name) {
  return String(element[name]);
}

function halogen_test_get_attribute(element, name) {
  return element.getAttribute(name);
}

function halogen_test_outer_html(element) {
  return element.outerHTML;
}

function halogen_test_is_visible(element) {
  return element.checkVisibility();
}

// 1 or 0: a number crosses both backends' pure imports the same way.
function halogen_test_same_element(a, b) {
  return a === b ? 1 : 0;
}

function halogen_test_is_connected(element) {
  return element.isConnected;
}

function halogen_test_is_null(value) {
  return value == null ? 1 : 0;
}

function halogen_test_length(array) {
  return array.length;
}

function halogen_test_index(array, index) {
  return array[index];
}
