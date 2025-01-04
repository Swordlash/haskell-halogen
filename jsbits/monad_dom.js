//#OPTIONS: CPP

function getNodeType(node) {
  return node.nodeType === 1 ? node.tagName : node.nodeType;
}

function js_create_text_node(text, document) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Creating text node with text: " + text);
#endif
  return document.createTextNode(text);
}

function js_set_text_content(text, node) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Setting text content to: " + text + " on node type: " + getNodeType(node));
#endif
  node.textContent = text;
}

function js_create_element(namespace, elemName, document) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Creating element with name: " + elemName);
#endif
  if (namespace === null) {
    return document.createElement(elemName);
  } else {
    return document.createElementNS(namespace, elemName);
  }
}

function js_insert_before(newNode, referenceNode, parentNode) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Inserting node type: " + getNodeType(newNode) + " before reference node type: " + getNodeType(referenceNode));
#endif
  parentNode.insertBefore(newNode, referenceNode);
}

function js_append_child(newNode, parentNode) {
  if (parentNode.lastChild !== newNode) {
#ifdef HALOGEN_TRACE_DOM
    console.log("Appending child node type: " + getNodeType(newNode) + " to parent node type: " + getNodeType(parentNode));
#endif
    parentNode.appendChild(newNode);
  } else {
#ifdef HALOGEN_TRACE_DOM
    console.log("Child node type: " + getNodeType(newNode) + " is already the last child of parent node type: " + getNodeType(parentNode));
#endif
  }
}

function js_replace_child(newNode, oldNode, parentNode) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Replacing old node type: " + getNodeType(oldNode) + " with new node type: " + getNodeType(newNode));
#endif
  parentNode.replaceChild(newNode, oldNode);
}

function js_insert_child_ix(index, newNode, parentNode) {
  var n = parentNode.childNodes.item(index) || null;
  if (n !== newNode) {
#ifdef HALOGEN_TRACE_DOM
    console.log("Inserting child node type: " + getNodeType(newNode) + " at index: " + index + " in parent node type: " + getNodeType(parentNode));
#endif
    parentNode.insertBefore(newNode, n);
  } else {
#ifdef HALOGEN_TRACE_DOM
    console.log("Node is already at index: " + index);
#endif
  }
}

function js_remove_child(childNode, parentNode) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Removing child node type: " + getNodeType(childNode) + " from parent node type: " + getNodeType(parentNode));
#endif
  parentNode.removeChild(childNode);
}

function js_parent_node(node) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Getting parent node of node type: " + getNodeType(node));
#endif
  return node.parentNode;
}

function js_next_sibling(node) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Getting next sibling of node type: " + getNodeType(node));
#endif
  return node.nextSibling;
}

function js_set_attribute(namespace, attrName, value, element) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Setting attribute: " + attrName + " with value: " + value + " on element type: " + getNodeType(element));
#endif
  if (namespace === null) {
    element.setAttribute(attrName, value);
  } else {
    element.setAttributeNS(namespace, attrName, value);
  }
}

function js_set_property(propName, propValue, element) {
  if (element[propName] !== propValue) {
#ifdef HALOGEN_TRACE_DOM
    console.log("Setting property: " + propName + " with value: " + propValue + " on element type: " + getNodeType(element));
#endif
    element[propName] = propValue;
  } else {
#ifdef HALOGEN_TRACE_DOM
    console.log("Property: " + propName + " is already set to: " + propValue);
#endif
  }
}

function js_unsafe_get_property(propName, element) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Getting property: " + propName + " from element type: " + getNodeType(element) + " value: " + element[propName]);
#endif
  return element[propName];
}

function js_remove_property(propName, element) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Removing property: " + propName + " from element type: " + getNodeType(element));
#endif
  delete element[propName];
}

function js_remove_attribute(namespace, attrName, element) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Removing attribute: " + attrName + " from element type: " + getNodeType(element));
#endif
  if (namespace === null) {
    element.removeAttribute(attrName);
  } else {
    element.removeAttributeNS(namespace, attrName);
  }
}

function js_has_attribute(namespace, attrName, element) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Checking if element type: " + getNodeType(element) + " has attribute: " + attrName);
#endif
  if (namespace === null) {
    return element.hasAttribute(attrName);
  } else {
    return element.hasAttributeNS(namespace, attrName);
  }
}

function js_add_event_listener(eventType, eventListener, eventTarget) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Adding event listener for " + eventType + " on target type: " + getNodeType(eventTarget));
#endif
  eventTarget.addEventListener(eventType, eventListener, false);
}

function js_remove_event_listener(eventType, eventListener, eventTarget) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Removing event listener for " + eventType + " from target type: " + getNodeType(eventTarget));
#endif
  eventTarget.removeEventListener(eventType, eventListener, false);
}

function js_get_window() {
#ifdef HALOGEN_TRACE_DOM
  console.log("Getting window");
#endif
  return window;
}

function js_get_document(window) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Getting document from window");
#endif
  return window.document;
}

function js_query_selector(selector, element) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Querying selector: " + selector + " on element type: " + getNodeType(element));
#endif
  return element.querySelector(selector);
}

function js_ready_state(element) {
#ifdef HALOGEN_TRACE_DOM
  console.log("Getting ready state of element type: " + getNodeType(element));
#endif
  return element.readyState;
}

function js_crypto_random_uuid() {
#ifdef HALOGEN_TRACE_DOM
  console.log("Generating random UUID");
#endif
  return window.crypto.randomUUID();
}

function js_unsafe_ref_eq(a, b) {
  return (a === b);
}

function js_current_target(e) {
  return e.target;
}