function h$js_create_text_node(text, document) {
  return document.createTextNode(text);
}

function h$js_set_text_content(text, node) {
  node.textContent = text;
}

function h$js_create_element(namespace, elemName, document) {
  if (namespace === null) {
    return document.createElement(elemName);
  } else {
    return document.createElementNS(namespace, elemName);
  }
}

function h$js_insert_before(newNode, referenceNode, parentNode) {
  parentNode.insertBefore(newNode, referenceNode);
}

function h$js_append_child(parentNode, newNode) {
  parentNode.appendChild(newNode);
}

function h$js_replace_child(newNode, oldNode, parentNode) {
  parentNode.replaceChild(newNode, oldNode);
}

function h$js_insert_child_ix(index, newNode, parentNode) {
  parentNode.insertBefore(newNode, parentNode.childNodes[index] || null);
}

function h$js_remove_child(parentNode, childNode) {
  parentNode.removeChild(childNode);
}

function h$js_parent_node(node) {
  return node.parentNode;
}

function h$js_next_sibling(node) {
  return node.nextSibling;
}

function h$js_set_attribute(namespace, attrName, value, element) {
  if (namespace === null) {
    element.setAttribute(attrName, value);
  } else {
    element.setAttributeNS(namespace, attrName, value);
  }
}

function h$js_set_property(propName, propValue, element) {
  element[propName] = propValue;
}

function h$js_unsafe_get_property(propName, element) {
  return element[propName];
}

function h$js_remove_property(propName, element) {
  delete element[propName];
}

function h$js_remove_attribute(namespace, attrName, element) {
  if (namespace === null) {
    element.removeAttribute(attrName);
  } else {
    element.removeAttributeNS(namespace, attrName);
  }
}

function h$js_has_attribute(namespace, attrName, element) {
  if (namespace === null) {
    return element.hasAttribute(attrName);
  } else {
    return element.hasAttributeNS(namespace, attrName);
  }
}

function h$js_add_event_listener(eventType, eventListener, eventTarget) {
  eventTarget.addEventListener(eventType, eventListener, false);
}

function h$js_remove_event_listener(eventType, eventListener, eventTarget) {
  eventTarget.removeEventListener(eventType, eventListener, false);
}

function h$js_get_window() {
  return window;
}

function h$js_get_document(window) {
  return window.document;
}

function h$js_query_selector(selector, element) {
  return element.querySelector(selector);
}

function h$js_ready_state(element) {
  return element.readyState;
}