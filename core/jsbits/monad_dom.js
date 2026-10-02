function js_create_text_node(text, document) {
  return document.createTextNode(text);
}

function js_set_text_content(text, node) {
  node.textContent = text;
}

function js_create_element(namespace, elemName, document) {
  if (namespace === null) {
    return document.createElement(elemName);
  } else {
    return document.createElementNS(namespace, elemName);
  }
}

function js_insert_before(newNode, referenceNode, parentNode) {
  if (newNode !== referenceNode.previousSibling) {
    parentNode.insertBefore(newNode, referenceNode);
  }
}

function js_append_child(newNode, parentNode) {
  if (parentNode.lastChild !== newNode) {
    parentNode.appendChild(newNode);
  }
}

function js_replace_child(newNode, oldNode, parentNode) {
  if (newNode !== oldNode) {
    parentNode.replaceChild(newNode, oldNode);
  }
}

function js_insert_child_ix(index, newNode, parentNode) {
  var n = parentNode.childNodes.item(index) || null;
  if (n !== newNode) {
    parentNode.insertBefore(newNode, n);
  }
}

function js_remove_child(childNode, parentNode) {
  parentNode.removeChild(childNode);
}

function js_parent_node(node) {
  return node.parentNode;
}

function js_next_sibling(node) {
  return node.nextSibling;
}

function js_set_attribute(namespace, attrName, value, element) {
  if (namespace === null) {
    element.setAttribute(attrName, value);
  } else {
    element.setAttributeNS(namespace, attrName, value);
  }
}

function js_set_property(propName, propValue, element) {
  if (element[propName] !== propValue) {
    element[propName] = propValue;
  }
}

function js_unsafe_get_property(propName, element) {
  return element[propName];
}

function js_remove_property(propName, element) {
  // A DOM property is an accessor inherited from the element's prototype, so
  // deleting it from the element does nothing. Set it back instead, as
  // purescript-halogen-vdom does.
  if (typeof element[propName] === "string") element[propName] = "";
  else if (propName === "rowSpan" || propName === "colSpan") element[propName] = 1;
  else element[propName] = undefined;
}

function js_remove_attribute(namespace, attrName, element) {
  if (namespace === null) {
    element.removeAttribute(attrName);
  } else {
    element.removeAttributeNS(namespace, attrName);
  }
}

function js_has_attribute(namespace, attrName, element) {
  if (namespace === null) {
    return element.hasAttribute(attrName);
  } else {
    return element.hasAttributeNS(namespace, attrName);
  }
}

function js_add_event_listener(eventType, eventListener, eventTarget) {
  eventTarget.addEventListener(eventType, eventListener, false);
}

function js_remove_event_listener(eventType, eventListener, eventTarget) {
  eventTarget.removeEventListener(eventType, eventListener, false);
}

function js_get_window() {
  return window;
}

function js_get_document(window) {
  return window.document;
}

function js_query_selector(selector, element) {
  return element.querySelector(selector);
}

function js_ready_state(element) {
  return element.readyState;
}



function js_unsafe_ref_eq(a, b) {
  return (a === b);
}



function js_property_equals(name, value, element) { return element[name] === value; }
