-- | The current window's own events, as emitters, through the global API
-- (@window.__TAURI__.window.getCurrentWindow()@): the events Tauri v2 emits
-- to a window when it is asked to close, gains or loses the focus, is
-- resized or moved, or its theme changes.
--
-- > data Action = Initialize | CloseRequested | Resized PhysicalSize
-- >
-- > handleAction = \case
-- >   Initialize -> do
-- >     void (H.subscribe (CloseRequested <$ Window.closeRequested))
-- >     void (H.subscribe (Resized <$> Window.resized))
-- >   CloseRequested -> do
-- >     _ <- invoke_ "save_draft" noArgs
-- >     void Window.destroy
-- >   Resized size -> modify $ \st -> st {size}
--
-- Each is an emitter as "Halogen.Tauri.Event"'s 'Halogen.Tauri.Event.listen'
-- is, with a listener on the current window rather than anywhere, so that
-- in an app with several windows a window hears its own events and not the
-- others'. Outside Tauri none of them ever fires.
--
-- Listening needs the event permissions (@core:event:default@, part of
-- @core:default@) in the window's capability, as every listener does;
-- 'destroy' needs @core:window:allow-destroy@, which no default set has.
module Halogen.Tauri.Window
  ( -- * Events
    closeRequested
  , focused
  , blurred
  , resized
  , moved
  , themeChanged

    -- * What they carry
  , PhysicalSize (..)
  , PhysicalPosition (..)
  , Theme (..)

    -- * Closing
  , destroy
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (..), Value, withText)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Halogen.Subscription qualified as HS
import Halogen.Tauri.Internal

-- | A size in physical pixels, as Tauri gives a window's inner size.
data PhysicalSize = PhysicalSize {width :: Int, height :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | A position in physical pixels, as Tauri gives a window's outer
-- position on the screen.
data PhysicalPosition = PhysicalPosition {x :: Int, y :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- | The window's theme, as Tauri names it.
data Theme = Light | Dark
  deriving stock (Eq, Show)

instance FromJSON Theme where
  parseJSON = withText "Theme" $ \case
    "light" -> pure Light
    "dark" -> pure Dark
    other -> fail ("a theme that is neither \"light\" nor \"dark\": " <> Text.unpack other)

-- | Someone asked the window to close (its close button, the system's
-- shortcut, the Rust side's @close@): @tauri:\/\/close-requested@.
--
-- While anything listens to it, Tauri does not close the window by itself:
-- a window whose page listens is taken to want a say, and closes only when
-- the program 'destroy's it. A component that subscribes to this has to,
-- once it has done what it wanted to do first (or decided not to close),
-- or the window cannot be closed but by quitting the app. (This is how
-- Tauri's own @onCloseRequested@ works: it destroys the window after its
-- handler unless the handler prevented that.)
closeRequested :: HS.Emitter IO ()
closeRequested = windowEvent "tauri://close-requested" (const () :: Value -> ())

-- | The window gained the focus: @tauri:\/\/focus@.
focused :: HS.Emitter IO ()
focused = windowEvent "tauri://focus" (const () :: Value -> ())

-- | The window lost the focus: @tauri:\/\/blur@.
blurred :: HS.Emitter IO ()
blurred = windowEvent "tauri://blur" (const () :: Value -> ())

-- | The window was resized, to this inner size: @tauri:\/\/resize@.
resized :: HS.Emitter IO PhysicalSize
resized = windowEvent "tauri://resize" id

-- | The window was moved, to this outer position: @tauri:\/\/move@.
moved :: HS.Emitter IO PhysicalPosition
moved = windowEvent "tauri://move" id

-- | The window's theme changed, the system's or the one the app set:
-- @tauri:\/\/theme-changed@.
themeChanged :: HS.Emitter IO Theme
themeChanged = windowEvent "tauri://theme-changed" id

-- | Close the current window at once, without emitting
-- @tauri:\/\/close-requested@ again, and wait until Tauri has done it: what
-- a program that listens to 'closeRequested' does once the window may
-- close. It waits, so it belongs in @lift@. Outside Tauri it gives
-- 'NotInTauri'.
destroy :: (MonadIO m) => m (Either TauriError ())
destroy = liftIO destroyCurrent

-- A payload that does not decode (which would be Tauri's change, not the
-- program's) is reported and dropped, as 'listen' does.
windowEvent :: (FromJSON p) => Text -> (p -> a) -> HS.Emitter IO a
windowEvent name f = HS.Emitter $ \handler ->
  HS.subscribe (listenTo CurrentWindow name) (either (reportPayload name) (handler . f))
