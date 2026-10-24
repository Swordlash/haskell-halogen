{-# LANGUAGE TemplateHaskell #-}

-- | Emitters for what happens to the page as a whole rather than to one of
-- a component's elements: keys pressed anywhere, the pointer moving over
-- anything, the window resized, the tab hidden, the network lost; and the
-- two clocks a page has, a timer and the screen's frames.
--
-- An element's events are handled where the element is rendered
-- ("Halogen.HTML.Events"). These have no element to be rendered on, so a
-- component subscribes to them, usually when it is initialized, and the
-- subscription ends with the component:
--
-- > handleAction = \case
-- >   Initialize -> do
-- >     keys <- H.liftEffect Window.keyDown
-- >     void $ H.subscribe (KeyPressed . KeyboardEvent.key <$> keys)
-- >     void $ H.subscribe (Tick <$ Window.interval 1000)
-- >   KeyPressed k -> ...
--
-- Every emitter here is an @'Emitter' 'IO'@, which is what @H.subscribe@
-- and the hooks' @subscribe@ take, so it is given to them as it is. Those
-- that listen to the page are made in the component's monad, which is what
-- speaks the page ('MonadBrowserDOM'), and are lowered to 'IO' with
-- 'lowerEmitter' as they are made; the two clocks need nothing of the page
-- and are plain values.
--
-- Make them with @H.liftEffect@ (in hooks, @Hooks.liftEffect@), as above,
-- rather than 'lift'. Making one does not wait, and 'lift' is where a
-- program may wait: what follows it runs later, on another thread, while
-- the tree goes on. A component counts as initialized once its
-- @Initialize@ has reached its first wait, and @runUI@ returns then, so
-- after a 'lift' the subscription is not yet there, and whatever the page
-- does until it is goes unheard. With @H.liftEffect@ it is there by the
-- time the component has been initialized.
--
-- Each subscription is its own listener, frame request or thread, made when
-- it is subscribed and taken away when it is unsubscribed, which the driver
-- does when the component goes away: a component that is gone hears nothing
-- more, and leaves nothing behind on the page.
--
-- Natively, on the in-memory DOM, the window and the document are one node
-- (see "Halogen.VDom.DOM.Monad.Native"): the listeners are added to it and
-- taken off it as in a browser, but nothing dispatches the page's events
-- there, the window measures 0 by 0, the page is visible and
-- 'animationFrame' never fires. 'interval' ticks everywhere.
--
-- The idea, ready-made subscriptions to the page's own events, is miso's
-- (@Miso.Subscription@); nothing here is its code.
module Halogen.Query.Event.Window
  ( -- * Any event
    windowEvent
  , documentEvent

    -- * The keyboard and the mouse
  , keyDown
  , keyUp
  , mouseMove
  , mouseDown
  , mouseUp

    -- * The page
  , resize
  , visibility
  , online

    -- * Clocks
  , interval
  , animationFrame
  )
where

import GHC.Clock (getMonotonicTimeNSec)
import HPrelude
import Halogen.JSBits (Callback, JSVal, Safety (..), browserJS, freeCallback, mkCallback)
import Halogen.Subscription (Emitter, lowerEmitter, makeEmitter)
import Halogen.VDom.DOM.Monad
import Web.DOM.Internal.Types (HTMLDocument (..))
import Web.Event.Event (Event, EventTarget (..), EventType)
import Web.HTML.Event.EventTypes qualified as ET
import Web.HTML.HTMLDocument (visibilityState)
import Web.HTML.HTMLDocument.VisibilityState (VisibilityState)
import Web.HTML.Window (innerHeight, innerWidth)
import Web.UIEvent.KeyboardEvent (KeyboardEvent (..))
import Web.UIEvent.KeyboardEvent.EventTypes qualified as KET
import Web.UIEvent.MouseEvent (MouseEvent (..))
import Web.UIEvent.MouseEvent.EventTypes qualified as MET

$( browserJS
     ["jsbits/web_browser.js"]
     [ ("js_request_animation_frame", "js_request_animation_frame", Unsafe, [t|Callback -> IO Int|])
     , ("js_cancel_animation_frame", "js_cancel_animation_frame", Unsafe, [t|Int -> IO ()|])
     , ("js_frame_time", "js_frame_time", Unsafe, [t|JSVal -> Double|])
     ]
 )

--------------------------------------------------------------------------------
-- Any event
--------------------------------------------------------------------------------

-- | The window's events of a type, those the function keeps.
--
-- The window hears an event dispatched to it (@resize@, @online@) and every
-- event that bubbles up through the document to it: a key pressed or a click
-- anywhere on the page, unless something on the way stopped its
-- propagation. It hears them last, after every element's own handler.
windowEvent
  :: (MonadBrowserDOM m, MonadUnliftIO m)
  => EventType
  -> (Event -> Maybe a)
  -> m (Emitter IO a)
windowEvent eventType f = lowerEmitter (listening windowTarget eventType (pure . f))

-- | The document's events of a type, those the function keeps: what is
-- dispatched to the document itself (@visibilitychange@, @selectionchange@)
-- and what bubbles up to it, just before the window hears it.
documentEvent
  :: (MonadBrowserDOM m, MonadUnliftIO m)
  => EventType
  -> (Event -> Maybe a)
  -> m (Emitter IO a)
documentEvent eventType f = lowerEmitter (listening documentTarget eventType (pure . f))

-- | A listener on a target found when it is subscribed, whose handler runs
-- in the page's monad, so that it can read the page as the event arrives.
-- The handler runs during the browser's dispatch of the event (see
-- 'mkEventListener'), which is what makes what it reads the page's state as
-- of the event, not as of whenever the component gets to it.
--
-- The target is found anew for each subscription rather than once, so an
-- emitter made before the page has finished loading still finds it. Its
-- unsubscribe can be run more than once, as can every one here.
listening
  :: (MonadBrowserDOM m, MonadIO m)
  => m EventTarget
  -> EventType
  -> (Event -> m (Maybe a))
  -> Emitter m a
listening which eventType f = makeEmitter $ \push -> do
  target <- which
  listener <- mkEventListener $ \ev -> traverse_ push =<< f ev
  addEventListener eventType listener target
  -- Only the first unsubscribe takes the listener off: in a browser that
  -- also frees its callback, and freeing it twice is an error.
  live <- newIORef True
  pure $ do
    was <- atomicModifyIORef' live (False,)
    when was (removeEventListener eventType listener target)

windowTarget :: (MonadBrowserDOM m) => m EventTarget
windowTarget = windowToEventTarget =<< window

-- The document is an event target as the window is. 'MonadBrowserDOM' has no
-- method saying so because its targets are the browser's own values (its
-- superclass says @DomEventTarget m ~ EventTarget@), and a document is one
-- as it is, as 'windowToEventTarget' takes the window as it is.
documentTarget :: (MonadBrowserDOM m) => m EventTarget
documentTarget = coerce <$> (document =<< window)

--------------------------------------------------------------------------------
-- The keyboard and the mouse
--------------------------------------------------------------------------------

-- | A key pressed anywhere on the page, and again while it is held down
-- (the browser's key repeat). 'Web.UIEvent.KeyboardEvent.key' says which.
-- The keys typed into a focused text field arrive here too, after the
-- field's own handlers have had them.
keyDown :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO KeyboardEvent)
keyDown = windowEvent KET.keydown (Just . KeyboardEvent)

-- | A key let go anywhere on the page.
keyUp :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO KeyboardEvent)
keyUp = windowEvent KET.keyup (Just . KeyboardEvent)

-- | The mouse moving over the page, as often as the browser reports it,
-- which can be once a frame: an action that renders for each is a render a
-- frame. 'Web.UIEvent.MouseEvent.clientX' and @clientY@ say where it is.
mouseMove :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO MouseEvent)
mouseMove = windowEvent MET.mousemove (Just . MouseEvent)

-- | A mouse button pressed anywhere on the page.
mouseDown :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO MouseEvent)
mouseDown = windowEvent MET.mousedown (Just . MouseEvent)

-- | A mouse button let go anywhere on the page, also outside the element it
-- was pressed on, which is what ends a drag that left its element.
mouseUp :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO MouseEvent)
mouseUp = windowEvent MET.mouseup (Just . MouseEvent)

--------------------------------------------------------------------------------
-- The page
--------------------------------------------------------------------------------

-- | The size of the viewport, its width and height in CSS pixels, each time
-- it changes. A subscription hears only changes: read the size it starts
-- from with "Web.HTML.Window"'s 'innerWidth' and 'innerHeight'.
--
-- The size is read as the event is dispatched, not when the component gets
-- to the action, so that each action carries the size that one resize made.
resize :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO (Int, Int))
resize = lowerEmitter $ listening windowTarget ET.resize $ \_ -> do
  w <- window
  width <- innerWidth w
  height <- innerHeight w
  pure (Just (width, height))

-- | Whether the page can be seen, each time that changes: its tab is
-- switched away from or back to, its window minimised or restored. A page
-- nobody sees can stop what is only there to be seen, such as an animation.
-- The state it starts from is "Web.HTML.HTMLDocument"'s 'visibilityState'.
--
-- The state is read from the document as the event is dispatched, which is
-- where the browser keeps it; the event itself does not say.
visibility :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO VisibilityState)
visibility = lowerEmitter $ listening documentTarget ET.visibilitychange $ \_ -> do
  Just <$> (visibilityState =<< document =<< window)

-- | 'True' when the browser goes online, 'False' when it goes offline: the
-- window's @online@ and @offline@ events. Online means only that the
-- browser has a network to use, not that any server can be reached on it;
-- a request that fails says more.
online :: (MonadBrowserDOM m, MonadUnliftIO m) => m (Emitter IO Bool)
online =
  lowerEmitter
    $ listening windowTarget ET.online (\_ -> pure (Just True))
    <|> listening windowTarget ET.offline (\_ -> pure (Just False))

--------------------------------------------------------------------------------
-- Clocks
--------------------------------------------------------------------------------

-- | A tick every so many milliseconds, from a thread of the subscription's
-- own that unsubscribing ends; a period under a millisecond is one.
--
-- The ticks keep to the clock the subscription started on rather than
-- waiting a whole period after each: a tick whose delivery took a while
-- does not push every later one back. A tick that comes due while the one
-- before it is still being delivered is skipped rather than delivered late,
-- as a browser's @setInterval@ does, so a slow component falls behind by
-- whole ticks instead of receiving a burst of them when it catches up.
--
-- It is a thread rather than the browser's @setInterval@ so that it runs
-- the same on every backend, natively too, where a test of a component
-- that ticks can wait for its ticks.
interval :: Int -> Emitter IO ()
interval period = makeEmitter $ \tick -> do
  start <- getMonotonicTimeNSec
  stopped <- newIORef False
  -- Forked unmasked: the driver may subscribe with exceptions masked, and
  -- a thread that inherited that would only die where it next waits.
  thread <- forkIOWithUnmask $ \unmask -> unmask (ticking stopped tick (start + step))
  pure $ do
    writeIORef stopped True
    -- A tick's action may run on the ticking thread itself (whichever
    -- thread brings the tree work while it is idle runs it), and may
    -- unsubscribe. Killing the thread then would cut that action short
    -- where it stands; the thread instead ends once the action is done.
    self <- myThreadId
    when (self /= thread) (killThread thread)
  where
    step = fromIntegral (max 1 period) * 1_000_000 :: Word64
    ticking stopped tick due = do
      now <- getMonotonicTimeNSec
      when (due > now) $ threadDelay (fromIntegral ((due - now + 999) `div` 1000))
      done <- readIORef stopped
      unless done $ do
        tick ()
        after <- getMonotonicTimeNSec
        -- The next tick still to come: those that came due meanwhile are
        -- skipped.
        let missed = if after > due then (after - due) `div` step else 0
        ticking stopped tick (due + step * (1 + missed))

-- | Each frame the browser draws, with its time: milliseconds since the
-- page started loading, as @performance.now()@ counts them, the same for
-- every callback of one frame. A frame is drawn as often as the screen
-- refreshes, usually 60 times a second, and not at all while the page is
-- hidden; this is the clock to animate by.
--
-- Each subscription asks for one frame at a time, asking for the next as
-- each comes, and unsubscribing cancels the one it is waiting for. A
-- component that renders on every frame renders as often as the screen
-- does, so keep what it renders small, or draw on a canvas
-- ("Halogen.Canvas").
--
-- Natively, and in an engine that draws no frames (Node, which runs the
-- test suites), there are no frames: it never fires.
animationFrame :: Emitter IO Double
animationFrame = makeEmitter $ \frame -> do
  -- The request waited for while subscribed; Nothing once unsubscribed. A
  -- frame that arrives then delivers nothing and asks for no other, which
  -- matters because its callback is freed by then.
  pending <- newMVar Nothing
  -- The callback, for itself to ask for the next frame with. It is written
  -- before the first frame is asked for, so before it can be called.
  self <- newIORef Nothing
  callback <- mkCallback $ \time -> do
    again <- readIORef self
    running <- modifyMVar pending $ \case
      Nothing -> pure (Nothing, False)
      Just _ -> do
        -- The next frame is asked for before this one is delivered, so a
        -- slow handler does not make the page miss one.
        next <- traverse js_request_animation_frame again
        pure (next, True)
    when running $ frame (js_frame_time time)
  writeIORef self (Just callback)
  modifyMVar_ pending $ \_ -> Just <$> js_request_animation_frame callback
  pure $ do
    -- Lock-step with the callback: once this has taken the request, no
    -- frame asks for another, and the callback can be freed. Only the first
    -- unsubscribe frees it, since freeing it twice is an error.
    subscribed <- modifyMVar pending $ \waiting ->
      (Nothing, isJust waiting) <$ traverse_ js_cancel_animation_frame waiting
    when subscribed (freeCallback callback)
