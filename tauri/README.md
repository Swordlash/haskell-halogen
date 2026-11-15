# haskell-halogen-tauri

Use [Tauri](https://tauri.app) v2 IPC from a [haskell-halogen](https://github.com/Swordlash/haskell-halogen) app in a Tauri webview. The package supports Rust commands, application events and current-window events through `window.__TAURI__`. Enable that global API in `tauri.conf.json`:

```json
{ "app": { "withGlobalTauri": true } }
```

The same program can run in a plain browser or natively. In those environments, `isTauri` returns `False`, commands return `Left NotInTauri`, event emitters never fire and `emit` does nothing.

## One `Main` for the browser and the webview

The webview loads the same `index.html` and `app.wasm` as a browser, from the `dist/` directory specified by `frontendDist`. You do not need a separate frontend build. Use `isTauri` to choose platform-specific behaviour, such as hiding a save button or downloading a file instead. You can check it in the component or pass the result from `Main`:

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

`invoke` uses aeson to encode arguments and decode results. The Rust side uses serde. Arguments must be a JSON object whose keys match the command parameters. Tauri converts parameter names to camelCase by default, so Rust's `note_id` becomes `noteId`. A Haskell record with camelCase fields and a generic `ToJSON` instance produces the expected keys.

For a command declared with `#[tauri::command(rename_all = "snake_case")]`, use `defaultOptions {fieldLabelModifier = camelTo2 '_'}`. This renaming applies only to command parameters. Structs passed as arguments or returned as results follow their own serde settings.

Use `encodeArgs` to inspect the generated JSON and `noArgs` for commands without parameters. Use `invoke_` when you do not need the result. This also handles Rust's `()` result, which becomes JSON `null` and cannot be decoded as aeson's `()`.

Failures are returned as values. `NotInTauri` means the program is running outside Tauri. `Rejected JSError` means the command returned `Err` or Tauri refused the call, for example because the command does not exist or the window lacks permission. String errors become the error message; other errors are represented as JSON. `BadResult` contains aeson's error message when the result cannot be decoded.

`invoke` waits for the result, so call it through `lift`. To send a command during an event's dispatch, call `start` through `H.liftEffect`, then wait for its `result` through `lift`.

Tauri cannot cancel commands. If the waiting component is removed, its wait ends, but the Rust function continues and its result is discarded.

## Events

```haskell
handleAction = \case
  Initialize -> void (H.subscribe (Progress <$> listen "download-progress"))
  Progress p -> modify $ \st -> st {progress = p}
  Done file -> emit "download-done" file
```

`listen name` returns an emitter for `H.subscribe`. Each subscription creates its own listener, which is removed when you unsubscribe or the component is disposed. Invalid payloads are logged to the console and skipped; use `listenEither` to receive the aeson decoding error instead. `emit` broadcasts an event, and `emitTo` sends it to a target label. Neither waits.

`Halogen.Tauri.Window` provides events for the current window: `closeRequested`, `focused`, `blurred`, `resized`, `moved` and `themeChanged`. While a `closeRequested` listener exists, Tauri keeps the window open until your program calls `destroy`. This matches Tauri's `onCloseRequested` behaviour and requires `core:window:allow-destroy` in the window's capabilities.

## Prerendered pages

A Tauri app serves static files from `dist/`, so you can prerender `index.html` with `Halogen.Static` and adopt it with `hydrateUI` in `Main`. The `vanilla` example demonstrates this. Prerendering needs no Tauri-specific support; `isTauri` returns `False` during native rendering.

## Tests

`Halogen-tauri-test` checks argument encoding and behaviour outside Tauri on every backend. `Halogen-tauri-browser-test` uses `hspec-halogen` to run in headless Chromium. It tests against `test/web/tauri-double.js`, which implements a command table and event bus based on Tauri v2's documented API. CI does not run Tauri itself.
