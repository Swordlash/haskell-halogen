# Changelog for `haskell-halogen-tauri`

## Unreleased

The first version, with Tauri v2's IPC for a Halogen app in a Tauri webview, through the global
API (`window.__TAURI__`, which the app has with `app.withGlobalTauri`).

`Halogen.Tauri.Invoke` calls the app's Rust commands. `invoke name args` encodes the arguments
with aeson, as one JSON object (Tauri's camelCase keys are what a Haskell record's fields give
with the default options), waits for the result and decodes it with aeson. A failure is a
`TauriError`: `NotInTauri` outside Tauri, `Rejected JSError` for a command's `Err` or a refusal
of Tauri's (an error serialised as anything but a string has its JSON as the message), and
`BadResult` with aeson's message for a result that does not decode. `invoke_` drops the result,
for a command that returns nothing; `noArgs` is the arguments of one without parameters, and
`encodeArgs` shows the JSON a record encodes to. `start` sends a command without waiting, for
`H.liftEffect`, and `result` waits for it later. Tauri cannot cancel a command, so a wait that is
killed only stops waiting.

`Halogen.Tauri.Event` has `listen`, an emitter of an event's decoded payloads for `H.subscribe`
(one that does not decode is reported to the console; `listenEither` delivers aeson's message
instead), whose unsubscribe unlistens, also when it comes before Tauri has registered the
listener; and `emit` and `emitTo`, which wait for nothing. `Halogen.Tauri.Window` has the
current window's own events (`closeRequested`, `focused`, `blurred`, `resized`, `moved`,
`themeChanged`), listened to on the window, and `destroy`, which closes it once a program that
listens to `closeRequested` lets it.

`Halogen.Tauri` re-exports the commands and the events, and `isTauri` says whether the program
runs where they work. Outside Tauri, in a plain browser and natively, `isTauri` is `False`,
every command gives `NotInTauri`, an emitter never fires and an emit does nothing, so one
program runs in a browser and in the webview.
