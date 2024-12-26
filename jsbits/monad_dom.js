function js_create_text_node(text, document) {
  // console.log("Creating text node with text: " + text);
  return document.createTextNode(text);
}

function js_set_text_content(text, node) {
  // console.log("Setting text content to: " + text);
  node.textContent = text;
}

function js_create_element(namespace, elemName, document) {
  // console.log("Creating element with name: " + elemName);
  if (namespace === null) {
    return document.createElement(elemName);
  } else {
    return document.createElementNS(namespace, elemName);
  }
}

function js_insert_before(newNode, referenceNode, parentNode) {
  // console.log("Inserting node before reference node");
  parentNode.insertBefore(newNode, referenceNode);
}

function js_append_child(newNode, parentNode) {
  // console.log("Appending child to parent");
  parentNode.appendChild(newNode);
}

function js_replace_child(newNode, oldNode, parentNode) {
  // console.log("Replacing child node");
  parentNode.replaceChild(newNode, oldNode);
}

function js_insert_child_ix(index, newNode, parentNode) {
  // console.log("Inserting child at index: " + index);
  parentNode.insertBefore(newNode, parentNode.childNodes[index] || null);
}

function js_remove_child(childNode, parentNode) {
  // console.log("Removing child from parent");
  parentNode.removeChild(childNode);
}

function js_parent_node(node) {
  // console.log("Getting parent node");
  return node.parentNode;
}

function js_next_sibling(node) {
  // console.log("Getting next sibling");
  return node.nextSibling;
}

function js_set_attribute(namespace, attrName, value, element) {
  // console.log("Setting attribute: " + attrName + " with value: " + value);
  if (namespace === null) {
    element.setAttribute(attrName, value);
  } else {
    element.setAttributeNS(namespace, attrName, value);
  }
}

function js_set_property(propName, propValue, element) {
  // console.log("Setting property: " + propName + " with value: " + propValue);
  element[propName] = propValue;
}

function js_unsafe_get_property(propName, element) {
  // console.log("Getting property: " + propName);
  return element[propName];
}

function js_remove_property(propName, element) {
  // console.log("Removing property: " + propName);
  delete element[propName];
}

function js_remove_attribute(namespace, attrName, element) {
  // console.log("Removing attribute: " + attrName);
  if (namespace === null) {
    element.removeAttribute(attrName);
  } else {
    element.removeAttributeNS(namespace, attrName);
  }
}

function js_has_attribute(namespace, attrName, element) {
  // console.log("Checking if element has attribute: " + attrName);
  if (namespace === null) {
    return element.hasAttribute(attrName);
  } else {
    return element.hasAttributeNS(namespace, attrName);
  }
}

function js_add_event_listener(eventType, eventListener, eventTarget) {
  // console.log("Adding event listener for " + eventType);
  eventTarget.addEventListener(eventType, eventListener, false);
}

function js_remove_event_listener(eventType, eventListener, eventTarget) {
  // console.log("Removing event listener for " + eventType);
  eventTarget.removeEventListener(eventType, eventListener, false);
}

function js_get_window() {
  // console.log("Getting window");
  return window;
}

function js_get_document(window) {
  // console.log("Getting document");
  return window.document;
}

function js_query_selector(selector, element) {
  // console.log("Querying selector: " + selector);
  return element.querySelector(selector);
}

function js_ready_state(element) {
  // console.log("Getting ready state");
  return element.readyState;
}

function js_crypto_random_uuid() {
  // console.log("Generating random UUID");
  return window.crypto.randomUUID();
}

function js_unsafe_ref_eq(a, b) {
  return (a === b);
}

function js_current_target(e) {
  return e.target;
}