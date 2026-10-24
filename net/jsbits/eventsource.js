// The browser's EventSource (server-sent events), for
// Halogen.Net.EventSource. The GHC JavaScript backend links this file and
// calls these by name; the wasm backend embeds it into its JSFFI module
// (Halogen.JSBits.browserJS).
//
// A source is made and given its listeners in one call, so that no event can
// come between the two: the browser dispatches a source's events as tasks of
// their own, after the one that made it. An event's name is the type it is
// dispatched as, so there is one listener for every name asked for, all
// calling the same Haskell callback; "message", the name of an event the
// server did not name, is always among them.
//
// A source stops for good when the page closes it, or when the browser gives
// up on it (a response that is no event stream, or an error status), which it
// reports as an error event with the source closed. Either way the listeners
// are taken off it here, so that the Haskell side can free its callbacks.

function halogen_net_eventsource_names() {
  return new Set(["message"]);
}

function halogen_net_eventsource_name_add(names, name) {
  names.add(name);
}

function halogen_net_eventsource_open(url, withCredentials, names, onOpen, onEvent, onError) {
  let source;
  try {
    source = new EventSource(url, { withCredentials });
  } catch (error) {
    return error;
  }
  const opened = () => onOpen(null);
  const event = (e) => onEvent(e);
  const failed = () => {
    // CLOSED: the browser will not reconnect.
    if (source.readyState === 2) halogen_net_eventsource_detach(source);
    onError(source);
  };
  source.addEventListener("open", opened);
  for (const name of names) source.addEventListener(name, event);
  source.addEventListener("error", failed);
  source.__halogenDetach = () => {
    source.removeEventListener("open", opened);
    for (const name of names) source.removeEventListener(name, event);
    source.removeEventListener("error", failed);
  };
  return source;
}

function halogen_net_eventsource_detach(source) {
  const detach = source.__halogenDetach;
  source.__halogenDetach = () => {};
  detach();
}

function halogen_net_eventsource_close(source) {
  halogen_net_eventsource_detach(source);
  source.close();
}

function halogen_net_eventsource_event_name(event) {
  return event.type;
}

function halogen_net_eventsource_event_data(event) {
  return String(event.data);
}

function halogen_net_eventsource_event_id(event) {
  return event.lastEventId;
}

function halogen_net_eventsource_ready_state(source) {
  return source.readyState;
}
