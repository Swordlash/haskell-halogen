-- | Tauri v2's IPC, for a Halogen app that runs in a Tauri webview: the
-- app's Rust commands ("Halogen.Tauri.Invoke") and its events
-- ("Halogen.Tauri.Event"), both through the global API
-- (@window.__TAURI__@), which the app has when it sets
-- @app.withGlobalTauri@ in its @tauri.conf.json@. The current window's own
-- events are in "Halogen.Tauri.Window", which is best imported qualified.
--
-- Outside Tauri, in a plain browser and natively, 'isTauri' is 'False', a
-- command gives 'NotInTauri', an emitter never fires and an emit does
-- nothing, so that one program can run in both and ask 'isTauri' only
-- where it has to do something else.
module Halogen.Tauri
  ( module Halogen.Tauri.Invoke
  , module Halogen.Tauri.Event
  )
where

import Halogen.Tauri.Event
import Halogen.Tauri.Invoke
