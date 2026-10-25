// What a page does to custom elements, for the suite (Test.Page): set their
// attributes and properties, call their methods, move and remove them,
// listen to their events, and look into their shadow roots. Elements are
// found by id in the document.

function halogen_ce_test_element(id) {
  const element = document.getElementById(id);
  if (!element) throw new Error(`no element #${id} in the page`);
  return element;
}

function halogen_ce_test_set_attribute(id, name, value) {
  halogen_ce_test_element(id).setAttribute(name, value);
}

function halogen_ce_test_set_property(id, name, json) {
  halogen_ce_test_element(id)[name] = JSON.parse(json);
}

function halogen_ce_test_get_property(id, name) {
  return JSON.stringify(halogen_ce_test_element(id)[name]) ?? "undefined";
}

// The element's method, called with the arguments of a JSON array: a
// promise of its result as JSON.
function halogen_ce_test_call(id, name, argsJson) {
  const element = halogen_ce_test_element(id);
  return element[name](...JSON.parse(argsJson)).then((result) => JSON.stringify(result) ?? "undefined");
}

// Set an attribute and call a method in the same task, with no await in
// between: what a page's script does when it configures an element and asks
// it something at once.
function halogen_ce_test_set_then_call(id, attribute, value, name) {
  const element = halogen_ce_test_element(id);
  element.setAttribute(attribute, value);
  return element[name]().then((result) => JSON.stringify(result) ?? "undefined");
}

function halogen_ce_test_has_method(id, name) {
  return typeof halogen_ce_test_element(id)[name] === "function";
}

function halogen_ce_test_move(id, to) {
  halogen_ce_test_element(to).appendChild(halogen_ce_test_element(id));
}

function halogen_ce_test_remove(id) {
  halogen_ce_test_element(id).remove();
}

// An element of the name, made from JavaScript as a framework makes one,
// with a property set before it is in the page (and perhaps before its name
// is defined), then put into the element with the id.
function halogen_ce_test_create(into, tag, id, property, json) {
  const element = document.createElement(tag);
  element.id = id;
  element[property] = JSON.parse(json);
  halogen_ce_test_element(into).appendChild(element);
}

function halogen_ce_test_shadow_text(id) {
  return halogen_ce_test_element(id).shadowRoot?.textContent ?? "(no shadow root)";
}

function halogen_ce_test_shadow_colour(id, selector) {
  const found = halogen_ce_test_element(id).shadowRoot?.querySelector(selector);
  return found ? getComputedStyle(found).color : "(nothing)";
}

function halogen_ce_test_shadow_click(id, selector) {
  halogen_ce_test_element(id).shadowRoot.querySelector(selector).click();
}

function halogen_ce_test_light_html(id) {
  return halogen_ce_test_element(id).innerHTML;
}

// What the document heard of an event, from now on: each one's detail,
// whether it bubbled and was composed, and its target's id.
function halogen_ce_test_listen(name) {
  if (!globalThis.halogenCeTestHeard) globalThis.halogenCeTestHeard = {};
  const heard = globalThis.halogenCeTestHeard;
  if (!heard[name]) {
    document.addEventListener(name, (event) => {
      heard[name].push({ detail: event.detail, bubbles: event.bubbles, composed: event.composed, target: event.target.id });
    });
  }
  heard[name] = [];
}

function halogen_ce_test_heard(name) {
  return JSON.stringify(globalThis.halogenCeTestHeard?.[name] ?? []);
}

function halogen_ce_test_dispatch(id, name, detailJson) {
  halogen_ce_test_element(id).dispatchEvent(new CustomEvent(name, { detail: JSON.parse(detailJson), bubbles: true }));
}
