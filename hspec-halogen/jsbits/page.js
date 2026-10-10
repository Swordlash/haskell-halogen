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
  return document.body.appendChild(Object.assign(document.createElement('div'), {className: 'halogen-test-root'}));
}

function halogen_test_create_container_in(parent) {
  return parent.appendChild(document.createElement('div'));
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

async function halogen_test_act(action, element, text) {
  const target = 'halogen-test-' + (globalThis.__halogenTestTargets = (globalThis.__halogenTestTargets ?? 0) + 1);
  element.setAttribute('data-halogen-test-target', target);
  try {
    const bridge = globalThis.__halogenTest;
    if (bridge) {
      await bridge.act(action, '[data-halogen-test-target="' + target + '"]', text);
    } else if (action === 'click') {
      element.click();
    } else {
      element.focus();
      element.value = action === 'type' ? element.value + text : '';
      element.dispatchEvent(new Event('input', {bubbles: true}));
    }
  } finally {
    element.removeAttribute('data-halogen-test-target');
  }
}

async function halogen_test_drag(source, target) {
  const counter = () => 'halogen-test-' + (globalThis.__halogenTestTargets = (globalThis.__halogenTestTargets ?? 0) + 1);
  const [from, to] = [counter(), counter()];
  source.setAttribute('data-halogen-test-target', from);
  target.setAttribute('data-halogen-test-target', to);
  try {
    const bridge = globalThis.__halogenTest;
    if (bridge?.drag) {
      await bridge.drag('[data-halogen-test-target="' + from + '"]', '[data-halogen-test-target="' + to + '"]');
    } else {
      // Pointer events for a drag the page follows itself, then HTML5 drag
      // and drop's, sharing one DataTransfer as a browser's would.
      const centre = (element) => {
        const box = element.getBoundingClientRect();
        return {clientX: box.left + box.width / 2, clientY: box.top + box.height / 2, bubbles: true, cancelable: true, isPrimary: true, pointerId: 1, button: 0};
      };
      source.dispatchEvent(new PointerEvent('pointerdown', {...centre(source), buttons: 1}));
      target.dispatchEvent(new PointerEvent('pointermove', {...centre(target), buttons: 1}));
      target.dispatchEvent(new PointerEvent('pointerup', centre(target)));
      const dataTransfer = new DataTransfer();
      source.dispatchEvent(new DragEvent('dragstart', {...centre(source), dataTransfer}));
      target.dispatchEvent(new DragEvent('dragenter', {...centre(target), dataTransfer}));
      target.dispatchEvent(new DragEvent('dragover', {...centre(target), dataTransfer}));
      target.dispatchEvent(new DragEvent('drop', {...centre(target), dataTransfer}));
      source.dispatchEvent(new DragEvent('dragend', {...centre(target), dataTransfer}));
    }
  } finally {
    source.removeAttribute('data-halogen-test-target');
    target.removeAttribute('data-halogen-test-target');
  }
}

async function halogen_test_press(key) {
  const bridge = globalThis.__halogenTest;
  if (bridge) {
    await bridge.press(key);
  } else {
    const target = document.activeElement ?? document.body;
    for (const type of ['keydown', 'keyup']) {
      target.dispatchEvent(new KeyboardEvent(type, {key, bubbles: true}));
    }
  }
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

function halogen_test_same_element(a, b) {
  return a === b;
}

function halogen_test_is_connected(element) {
  return element.isConnected;
}

function halogen_test_is_null(value) {
  return value == null;
}

function halogen_test_length(array) {
  return array.length;
}

function halogen_test_index(array, index) {
  return array[index];
}
