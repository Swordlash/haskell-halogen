var ENABLE_LOGGING = false; // Set this to false to disable logging

function log(message) {
  if (ENABLE_LOGGING) {
    console.log(message);
  }
}

function getNodeType(node) {
  return node.nodeType === 1 ? node.tagName : node.nodeType;
}

function js_create_text_node(text, document) {
  log("Creating text node with text: " + text);
  return document.createTextNode(text);
}

function js_set_text_content(text, node) {
  log("Setting text content to: " + text + " on node type: " + getNodeType(node));
  node.textContent = text;
}

function js_create_element(namespace, elemName, document) {
  log("Creating element with name: " + elemName);
  if (namespace === null) {
    return document.createElement(elemName);
  } else {
    return document.createElementNS(namespace, elemName);
  }
}

function js_insert_before(newNode, referenceNode, parentNode) {
  log("Inserting node type: " + getNodeType(newNode) + " before reference node type: " + getNodeType(referenceNode));
  parentNode.insertBefore(newNode, referenceNode);
}

function js_append_child(newNode, parentNode) {
  if (parentNode.lastChild !== newNode) {
    log("Appending child node type: " + getNodeType(newNode) + " to parent node type: " + getNodeType(parentNode));
    parentNode.appendChild(newNode);
  } else {
    log("Child node type: " + getNodeType(newNode) + " is already the last child of parent node type: " + getNodeType(parentNode));
  }
}

function js_replace_child(newNode, oldNode, parentNode) {
  log("Replacing old node type: " + getNodeType(oldNode) + " with new node type: " + getNodeType(newNode));
  parentNode.replaceChild(newNode, oldNode);
}

function js_insert_child_ix(index, newNode, parentNode) {
  var n = parentNode.childNodes.item(index) || null;
  if (n !== newNode) {
    log("Inserting child node type: " + getNodeType(newNode) + " at index: " + index + " in parent node type: " + getNodeType(parentNode));
    parentNode.insertBefore(newNode, n);
  } else {
    log("Node is already at index: " + index);
  }
}

function js_remove_child(childNode, parentNode) {
  log("Removing child node type: " + getNodeType(childNode) + " from parent node type: " + getNodeType(parentNode));
  parentNode.removeChild(childNode);
}

function js_parent_node(node) {
  log("Getting parent node of node type: " + getNodeType(node));
  return node.parentNode;
}

function js_next_sibling(node) {
  log("Getting next sibling of node type: " + getNodeType(node));
  return node.nextSibling;
}

function js_set_attribute(namespace, attrName, value, element) {
  log("Setting attribute: " + attrName + " with value: " + value + " on element type: " + getNodeType(element));
  if (namespace === null) {
    element.setAttribute(attrName, value);
  } else {
    element.setAttributeNS(namespace, attrName, value);
  }
}

function js_set_property(propName, propValue, element) {
  if (element[propName] !== propValue) {
    log("Setting property: " + propName + " with value: " + propValue + " on element type: " + getNodeType(element));
    element[propName] = propValue;
  } else {
    log("Property: " + propName + " is already set to: " + propValue);
  }
}

function js_unsafe_get_property(propName, element) {
  log("Getting property: " + propName + " from element type: " + getNodeType(element) + " value: " + element[propName]);
  return element[propName];
}

function js_remove_property(propName, element) {
  log("Removing property: " + propName + " from element type: " + getNodeType(element));
  delete element[propName];
}

function js_remove_attribute(namespace, attrName, element) {
  log("Removing attribute: " + attrName + " from element type: " + getNodeType(element));
  if (namespace === null) {
    element.removeAttribute(attrName);
  } else {
    element.removeAttributeNS(namespace, attrName);
  }
}

function js_has_attribute(namespace, attrName, element) {
  log("Checking if element type: " + getNodeType(element) + " has attribute: " + attrName);
  if (namespace === null) {
    return element.hasAttribute(attrName);
  } else {
    return element.hasAttributeNS(namespace, attrName);
  }
}

function js_add_event_listener(eventType, eventListener, eventTarget) {
  log("Adding event listener for " + eventType + " on target type: " + getNodeType(eventTarget));
  eventTarget.addEventListener(eventType, eventListener, false);
}

function js_remove_event_listener(eventType, eventListener, eventTarget) {
  log("Removing event listener for " + eventType + " from target type: " + getNodeType(eventTarget));
  eventTarget.removeEventListener(eventType, eventListener, false);
}

function js_get_window() {
  log("Getting window");
  return window;
}

function js_get_document(window) {
  log("Getting document from window");
  return window.document;
}

function js_query_selector(selector, element) {
  log("Querying selector: " + selector + " on element type: " + getNodeType(element));
  return element.querySelector(selector);
}

function js_ready_state(element) {
  log("Getting ready state of element type: " + getNodeType(element));
  return element.readyState;
}

function js_crypto_random_uuid() {
  log("Generating random UUID");
  return window.crypto.randomUUID();
}

function js_unsafe_ref_eq(a, b) {
  return (a === b);
}

function js_current_target(e) {
  return e.target;
}