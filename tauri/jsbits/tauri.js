// Tauri v2's IPC, for Halogen.Tauri. The GHC JavaScript backend links this
// file and calls these by name; the wasm backend embeds it into its JSFFI
// module (Halogen.JSBits.browserJS).
//
// Everything goes through window.__TAURI__, the global API a Tauri app's
// webview has when the app sets app.withGlobalTauri. Values cross as JSON
// text: the Haskell side encodes and decodes them with aeson, as the Rust
// side does with serde, and here they are only parsed and stringified.
//
// Every promise returned here has a rejection handler attached that does
// nothing, as net's do: a command can be started and its result dropped (a
// component that goes away stops waiting for it), and the engine would
// otherwise report each such rejection as unhandled. Whoever awaits the
// promise still gets the rejection.

function halogen_tauri_quiet(promise) {
  promise.catch(() => {});
  return promise;
}

// Whether the global API is here with what these bindings call. A Tauri app
// without app.withGlobalTauri has Tauri's internals but no __TAURI__, and
// these bindings cannot work there, so it counts as no Tauri at all.
function halogen_tauri_is_tauri() {
  const api = globalThis.__TAURI__;
  return typeof api?.core?.invoke === "function" && typeof api?.event?.listen === "function";
}

// What a command is rejected with, as Halogen.JSBits.Promise reads it. A
// Rust command's error is whatever serde made of it: a string (a
// Result<_, String>, and Tauri's own refusals, such as a command that does
// not exist or that the window's capabilities do not allow) is kept as it
// is, and comes back as the message of an "Error"; an Error is kept too;
// anything else (an error enum or struct serialised as an object, say) is
// an "Error" whose message is that value's JSON, for the program to decode.
function halogen_tauri_rejection(error) {
  if (error instanceof Error || typeof error === "string") return error;
  let message;
  try {
    message = JSON.stringify(error) ?? String(error);
  } catch (_) {
    message = String(error);
  }
  return { name: "Error", message };
}

function halogen_tauri_json(value) {
  return JSON.stringify(value === undefined ? null : value);
}

// Call the command with the arguments, which are JSON text, and return the
// promise of its result as JSON text. Tauri takes a command's arguments as
// an object keyed by the arguments' names; it reads an array of numbers as
// raw bytes instead, so anything but an object is refused here, with a
// TypeError, rather than sent as something else than was meant.
function halogen_tauri_invoke(command, args) {
  let parsed;
  try {
    parsed = JSON.parse(args);
    if (parsed === null || typeof parsed !== "object" || Array.isArray(parsed)) {
      throw new TypeError("a command's arguments must be a JSON object, not " + args);
    }
  } catch (error) {
    return halogen_tauri_quiet(Promise.reject(error));
  }
  try {
    return halogen_tauri_quiet(
      Promise.resolve(globalThis.__TAURI__.core.invoke(command, parsed)).then(halogen_tauri_json, (error) => {
        throw halogen_tauri_rejection(error);
      }),
    );
  } catch (error) {
    return halogen_tauri_quiet(Promise.reject(halogen_tauri_rejection(error)));
  }
}

function halogen_tauri_report(what, error) {
  console.error("Halogen.Tauri: " + what + ":", error);
}

// Listen to the event, anywhere (event.listen) or on the current window
// only (the window's own listen, which the window's events need), and call
// the callback with each event's payload, as JSON text.
//
// listen gives a promise of the function that unlistens, and the program
// may unsubscribe before it has resolved: the subscription then notes that
// it has stopped, and unlistens as soon as the promise resolves. A stopped
// subscription calls the callback no more, whatever is still on its way, so
// the Haskell side can free the callback when it unsubscribes. A listen the
// API refuses (an event name with characters Tauri does not allow, say) has
// nobody to tell, and is reported to the console.
function halogen_tauri_listen(name, currentWindow, callback) {
  const subscription = { unlisten: null, stopped: false };
  const handler = (event) => {
    if (!subscription.stopped) callback(halogen_tauri_json(event?.payload));
  };
  let listening;
  try {
    const api = globalThis.__TAURI__;
    listening = currentWindow ? api.window.getCurrentWindow().listen(name, handler) : api.event.listen(name, handler);
  } catch (error) {
    listening = Promise.reject(error);
  }
  Promise.resolve(listening).then(
    (unlisten) => {
      if (subscription.stopped) halogen_tauri_unlistening(name, unlisten);
      else subscription.unlisten = unlisten;
    },
    (error) => {
      if (!subscription.stopped) halogen_tauri_report("could not listen to " + name, error);
    },
  );
  return subscription;
}

function halogen_tauri_unlistening(name, unlisten) {
  try {
    Promise.resolve(unlisten()).catch((error) => halogen_tauri_report("could not unlisten " + name, error));
  } catch (error) {
    halogen_tauri_report("could not unlisten " + name, error);
  }
}

function halogen_tauri_unlisten(name, subscription) {
  subscription.stopped = true;
  const unlisten = subscription.unlisten;
  subscription.unlisten = null;
  if (unlisten !== null) halogen_tauri_unlistening(name, unlisten);
}

// Emit the event, with its payload as JSON text, to every listener
// (event.emit) or to those of one target, named by its label
// (event.emitTo). Nobody waits for it, so a refusal is reported to the
// console.
function halogen_tauri_emit(name, payload) {
  halogen_tauri_emitting(name, payload, (event, parsed) => event.emit(name, parsed));
}

function halogen_tauri_emit_to(target, name, payload) {
  halogen_tauri_emitting(name, payload, (event, parsed) => event.emitTo(target, name, parsed));
}

function halogen_tauri_emitting(name, payload, send) {
  try {
    Promise.resolve(send(globalThis.__TAURI__.event, JSON.parse(payload))).catch((error) =>
      halogen_tauri_report("could not emit " + name, error),
    );
  } catch (error) {
    halogen_tauri_report("could not emit " + name, error);
  }
}

// A decode failure of an event's payload, which has nobody to tell either.
function halogen_tauri_report_payload(name, message) {
  halogen_tauri_report("an event " + name + " whose payload does not decode", message);
}

// Destroy the current window, closing it without asking again: what a
// program that listens to tauri://close-requested does once it lets the
// window close.
function halogen_tauri_destroy() {
  try {
    return halogen_tauri_quiet(
      Promise.resolve(globalThis.__TAURI__.window.getCurrentWindow().destroy()).then(
        () => null,
        (error) => {
          throw halogen_tauri_rejection(error);
        },
      ),
    );
  } catch (error) {
    return halogen_tauri_quiet(Promise.reject(halogen_tauri_rejection(error)));
  }
}
