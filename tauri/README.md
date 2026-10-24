# haskell-halogen-tauri

[Tauri](https://tauri.app) v2's IPC for a [haskell-halogen](https://github.com/Swordlash/haskell-halogen)
app that runs in a Tauri webview: the app's Rust commands, its events, and the current window's
own events. Everything goes through Tauri's global API, `window.__TAURI__`, so the app sets
`app.withGlobalTauri` in its `tauri.conf.json`:

```json
{ "app": { "withGlobalTauri": true } }
```

The same program runs in a plain browser too. There `isTauri` is `False`, every command gives
`Left NotInTauri`, an emitter never fires and `emit` does nothing; natively the same.

## One `Main` for the browser and the webview

The webview loads the same `index.html` and `app.wasm` a browser does, out of the `dist/` that
`frontendDist` names, so nothing about the build changes. A component that should behave
differently in a browser (no save button, say, or one that downloads instead) asks `isTauri`,
or is told by `Main`:

```haskell
module Main (main) where

import Example.Notes (Platform (..), notes)
import Halogen.IO.Util qualified as HA
import Halogen.Tauri (isTauri)
import Halogen.VDom.DOM.Monad (runBrowserDOM)
import Halogen.VDom.Driver (runUI)

main :: IO ()
main = runBrowserDOM $ do
  HA.awaitLoad
  tauri <- isTauri
  body <- HA.awaitBody
  _ <- runUI notes (if tauri then Desktop else Web) body
  pure ()
```

## Commands

```haskell
import Halogen.Tauri

-- #[tauri::command]
-- fn save_note(note_id: i64, text: String) -> Result<Note, String>
data SaveNote = SaveNote {noteId :: Int, text :: Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

handleAction = \case
  -- Waits on a worker thread, while the rest of the tree goes on.
  Save -> do
    st <- get
    outcome <- invoke "save_note" SaveNote {noteId = st.noteId, text = st.text}
    modify $ \st' -> st' {saved = outcome}
```

`invoke` encodes the arguments with aeson and decodes the result with aeson, as the Rust side
uses serde. The arguments are one JSON object keyed by the command's parameters, which Tauri
renames to camelCase by default (`note_id` is `noteId`): a Haskell record's fields are camelCase
already, so a plain `Generic` `ToJSON` instance with the default options gives the keys Tauri
expects. A command declared with `#[tauri::command(rename_all = "snake_case")]` wants
`defaultOptions {fieldLabelModifier = camelTo2 '_'}` instead. Only the parameters are renamed: a
struct passed as one, or returned, is (de)serialised by serde as the struct says. `encodeArgs`
shows the JSON a record encodes to; `noArgs` is the arguments of a command without parameters;
`invoke_` calls one whose result does not matter (Rust's `()` is `null`, which aeson's `()`
does not read).

A failure is a value: `NotInTauri` outside Tauri, `Rejected JSError` for an `Err` of the
command or a refusal of Tauri's (a command that does not exist, or that the window's capabilities
do not allow), with a string error as its `message` and any other error as its JSON, and
`BadResult` with aeson's message for a result that does not decode.

`invoke` waits, so it belongs in `lift`. A command that must be sent inside an event's dispatch
is sent with `start` in `H.liftEffect`, and its `result` waited for in `lift`. Tauri has no way to
cancel a command: killing the wait (a component that goes away) stops the wait, the Rust function
runs to its end, and its result is dropped.

## Events

```haskell
handleAction = \case
  Initialize -> void (H.subscribe (Progress <$> listen "download-progress"))
  Progress p -> modify $ \st -> st {progress = p}
  Done file -> emit "download-done" file
```

`listen name` is an emitter, as `H.subscribe` takes it; each subscription is a listener of its
own, and unsubscribing (or the component going away) unlistens it. A payload that does not
decode is reported to the console and not delivered; `listenEither` delivers aeson's message
instead. `emit` and `emitTo` (a target label) send an event without waiting.

`Halogen.Tauri.Window` has the current window's own events, listened to on the window:
`closeRequested`, `focused`, `blurred`, `resized`, `moved` and `themeChanged`. While anything
listens to `closeRequested`, Tauri leaves the window open and the program closes it with
`destroy` (which needs `core:window:allow-destroy` in the window's capability), as Tauri's own
`onCloseRequested` does.

## Prerendered pages

A Tauri app's `dist/` is a static site like any other, so `Halogen.Static` can write its
`index.html` with the component already rendered into it, and `Main` mount it with `hydrateUI`
instead of `runUI`, which adopts that HTML, as the `vanilla` example does. Nothing here is
needed for that, and `isTauri` is `False` natively, where the page is rendered.

## Tests

`Halogen-tauri-test` checks the encoding of arguments and what everything does outside Tauri, on
every backend. `Halogen-tauri-browser-test` runs in headless Chromium through the
`hspec-halogen` runner against `test/web/tauri-double.js`, a stand-in for `window.__TAURI__`
with a table of commands and an event bus. There is no Tauri in CI, so the package is tested
against that double, written from Tauri v2's documented API, and not against Tauri itself.
