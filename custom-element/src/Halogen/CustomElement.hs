{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | A Halogen component as a custom element (a Web Component), for any page
-- to use as it uses @\<video>@: from plain HTML, React, Vue, Svelte, Astro or
-- HTMX, none of which needs to know that Halogen is behind it. And the other
-- way, what a Halogen component needs to use custom elements made elsewhere.
--
-- > import Halogen.CustomElement qualified as CE
-- > import Halogen.CustomElement (Definition (..))
-- >
-- > main = runBrowserDOM $ do
-- >   HA.endPreviousRun
-- >   registration <-
-- >     CE.define "x-counter"
-- >       CE.defaultDefinition
-- >         { input = \props -> fromMaybe 0 (CE.prop "start" props)
-- >         , observed = ["start"]
-- >         , output = \n -> Just ("count", toJSON n)
-- >         , methods = [CE.tellMethod "reset" (\() -> Reset)]
-- >         }
-- >       counter
-- >   HA.keepForNextRun registration.dispose
--
-- after which the page can write @\<x-counter start="5">\</x-counter>@, set
-- @counter.start = 5@ from JavaScript, listen to its @count@ events and call
-- @counter.reset()@.
--
-- = How an element runs its component
--
-- Each element of the name, once it is in the document, mounts a tree of its
-- own with 'Halogen.VDom.Driver.runUI', into its shadow root (or into itself,
-- with 'NoShadow'). The tree's root is not the component itself but a small
-- parent around it, which renders it in a slot with the element's 'input':
-- so when an attribute or a property the definition 'observed' changes, the
-- component hears it as @Receive@ (see 'Halogen.onReceive'), exactly as it
-- would under a Halogen parent that rendered it with a new input. What the
-- component raises is dispatched on the element as a @CustomEvent@, and a
-- 'Method' is an element method that sends the component a query.
--
-- = An element's life
--
-- An element mounts its component when it is connected to the document
-- (written in the page's HTML, inserted from JavaScript, rendered by a
-- framework), and disposes of it, running its finalizers and ending its
-- subscriptions and forks, when it is removed. The removal is acted on after
-- the code that removed it has run (in a microtask), and not at all if that
-- code inserted the element again: an element moved from one place to
-- another, as a framework moves the items of a list, keeps its component and
-- its state.
--
-- @customElements.define@ is forever: a page cannot take a name back or give
-- it another class. 'define' can be called again, though, for the same name,
-- with another definition and another component: the elements of the name
-- already in the page dispose of their components and mount the new one, and
-- elements connected afterwards mount the new one too. That is what a
-- program run again in the same page needs (browser GHCi's @:main@, the
-- tutorial's next compile). 'Registration''s 'dispose' takes the definition
-- back as far as a page lets it: every element disposes of its component,
-- and the name is inert, its elements rendering nothing, until it is defined
-- again.
--
-- What the page does to an element (connecting it, setting what it
-- observes, calling a method) reaches Haskell through the same synchronous
-- callbacks as the browser's events: Haskell has seen it by the time the
-- page's code goes on, though what the component does about it may come
-- later. An attribute or a property set from inside the component's own
-- @H.liftEffect@ queues its input behind that effect, as any outside
-- caller's query does.
--
-- = Natively
--
-- Natively there is no page: 'define' checks the name and returns a
-- registration whose 'dispose' does nothing, and no element ever mounts.
module Halogen.CustomElement
  ( -- * Defining an element
    define
  , Registration (..)
  , Definition (..)
  , defaultDefinition
  , Shadow (..)

    -- * What the page sets on the element
  , Props
  , attr
  , prop

    -- * Methods
  , Method
  , method
  , tellMethod

    -- * Using others' custom elements
  , onEvent

    -- * Failures
  , JSError (..)
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, withMVar)
import Control.Exception (SomeException, displayException, mask, onException, throwIO, try)
import Control.Monad (join, unless, void, when, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (put)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecodeStrictText, encode, toJSON)
import Data.Aeson.Types (parseJSON, parseMaybe)
import Data.ByteString.Lazy qualified as Lazy
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Row (type (.==))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties (IProp)
import Halogen.JSBits (Callback, JSText, JSVal, Safety (..), browserJS, freeCallback, fromJSText, mkSyncCallback, toJSText)
import Halogen.JSBits.Promise (JSError (..))
import Halogen.VDom.DOM.Monad (MonadBrowserDOM)
import Halogen.VDom.Driver (runUI)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (MonadUnliftIO, withRunInIO)
import Web.DOM.Internal.Types (HTMLElement (..))
import Web.Event.Event (EventType (..))
import Web.Event.Internal.Types (Event (..))

$( browserJS
     ["jsbits/custom-element.js"]
     [ ("js_define", "halogen_custom_element_define", Unsafe, [t|JSText -> Int -> JSText -> JSText -> Int -> JSText -> Callback -> Callback -> Callback -> Callback -> Callback -> IO JSVal|])
     , ("js_fresh_token", "halogen_custom_element_fresh_token", Unsafe, [t|IO Int|])
     , ("js_inert", "halogen_custom_element_inert", Unsafe, [t|JSText -> Int -> IO ()|])
     , ("js_instance_id", "halogen_custom_element_instance_id", Unsafe, [t|JSVal -> IO Int|])
     , ("js_mount_point", "halogen_custom_element_mount_point", Unsafe, [t|JSVal -> IO JSVal|])
     , ("js_props", "halogen_custom_element_props", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_dispatch", "halogen_custom_element_dispatch", Unsafe, [t|JSVal -> JSText -> JSText -> IO ()|])
     , ("js_length", "halogen_custom_element_length", Unsafe, [t|JSVal -> IO Int|])
     , ("js_index", "halogen_custom_element_index", Unsafe, [t|JSVal -> Int -> IO JSVal|])
     , ("js_is_error", "halogen_custom_element_is_error", Unsafe, [t|JSVal -> IO Bool|])
     , ("js_error_name", "halogen_custom_element_error_name", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_error_message", "halogen_custom_element_error_message", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_call_element", "halogen_custom_element_call_element", Unsafe, [t|JSVal -> IO JSVal|])
     , ("js_call_name", "halogen_custom_element_call_name", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_call_args", "halogen_custom_element_call_args", Unsafe, [t|JSVal -> IO JSText|])
     , ("js_resolve", "halogen_custom_element_resolve", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_reject", "halogen_custom_element_reject", Unsafe, [t|JSVal -> JSText -> IO ()|])
     , ("js_detail", "halogen_custom_element_detail", Unsafe, [t|JSVal -> JSText|])
     ]
 )

--------------------------------------------------------------------------------
-- Defining an element
--------------------------------------------------------------------------------

-- | What makes an element of a component: how its input comes from the
-- element, what of its output becomes events, which queries are methods,
-- and how it is rendered. Start from 'defaultDefinition' and set what the
-- element needs, with the fields in scope (@import Halogen.CustomElement
-- (Definition (..))@):
--
-- > CE.defaultDefinition
-- >   { input = \props -> fromMaybe "world" (CE.attr "name" props)
-- >   , observed = ["name"]
-- >   }
--
-- Setting 'input' changes the definition's input type from
-- 'defaultDefinition''s @()@ to the component's.
data Definition query input output = Definition
  { input :: Props -> input
  -- ^ The component's input, from what the page has set of the 'observed'
  -- names. Asked when the element mounts its component, and again whenever
  -- one of them changes, which the component hears as @Receive@.
  , observed :: [Text]
  -- ^ The names whose attributes and properties 'input' reads. Each is
  -- watched as an attribute, and is a property of the element too (a getter
  -- giving what was last set, a setter that sets it), unless it is one
  -- @HTMLElement@ has already, such as @title@ or @hidden@, which stays the
  -- browser's: then it is an attribute only. Attributes are matched as HTML
  -- matches them, in any case, and properties exactly.
  , output :: output -> Maybe (Text, Value)
  -- ^ The event an output becomes, if any: its name and its @detail@, as
  -- JSON. It is dispatched on the element as a @CustomEvent@ that bubbles
  -- and is @composed@, so that it leaves a shadow root its element is in,
  -- and the page can listen for it anywhere above.
  , methods :: [Method query]
  -- ^ The element's methods, each sending the component a query.
  , shadow :: Shadow
  -- ^ Where the component renders.
  , styles :: Text
  -- ^ CSS for the shadow root, adopted into it as one stylesheet shared by
  -- every element of the name: it styles the component and nothing outside
  -- it, and the page's own CSS does not reach in. Empty for none, and not
  -- used with 'NoShadow', where the page's CSS styles the component.
  }

-- | An element with no input, which is silent, has no methods, and renders
-- into an open shadow root with no styles of its own.
defaultDefinition :: forall query output. Definition query () output
defaultDefinition =
  Definition
    { input = const ()
    , observed = []
    , output = const Nothing
    , methods = []
    , shadow = OpenShadow
    , styles = ""
    }

-- | Where an element's component renders.
data Shadow
  = -- | In a shadow root the page can reach as the element's @shadowRoot@
    -- (and which tests and devtools can look into).
    OpenShadow
  | -- | In a shadow root the page cannot reach.
    ClosedShadow
  | -- | In the element itself, among the page's own elements, styled by the
    -- page's CSS. A child the page put in the element stays, before the
    -- component's HTML.
    NoShadow
  deriving stock (Eq, Show)

-- | How to take a definition back: 'dispose' disposes of the component of
-- every element of the name, and leaves the name inert until it is defined
-- again. That is what 'Halogen.IO.Util.keepForNextRun' wants, so that the
-- next run of a program can end this one's elements before defining its own.
-- Disposing twice, or after the name has been defined again, does nothing
-- more.
newtype Registration m = Registration {dispose :: m ()}

-- | Define a custom element whose content is the component, under a name
-- that must hold a hyphen and start with a lower-case letter, as the browser
-- requires (@x-counter@, @todo-list@). A name the browser refuses, or one
-- that something other than this module has defined, is thrown as a
-- 'JSError' (a @SyntaxError@, a @NotSupportedError@).
--
-- Defining a name again takes it over: see "An element's life" above.
-- Elements of the name already in the page are mounted before this returns.
define
  :: forall query input output m
   . (MonadBrowserDOM m, MonadUnliftIO m)
  => Text
  -> Definition query input output
  -> H.Component query input output m
  -> m (Registration m)
define name definition component = withRunInIO $ \run -> do
  for_ (badName name) throwIO
  -- The definition this one takes over goes first, its elements' components
  -- with it.
  previous <- atomicModifyIORef' registry (\r -> (Map.delete name r, Map.lookup name r))
  for_ previous snd
  -- The token comes from the page's registry, not from this program: two
  -- programs in one page (the tutorial's app and the program it compiled,
  -- or a run of browser GHCi after another) share the registry, and a
  -- disposal of one's registration must not be taken for the other's.
  token <- js_fresh_token
  instances <- newIORef IntMap.empty
  let mountElement element = do
        key <- js_instance_id element
        lock <- newEmptyMVar
        fresh <- atomicModifyIORef' instances $ \is ->
          if IntMap.member key is then (is, False) else (IntMap.insert key (Instance lock) is, True)
        when fresh $ do
          -- The lock is held while the tree mounts, so that a change
          -- arriving meanwhile waits for it, and then reads what is set
          -- by then.
          outcome <- try (mountOn run definition component element)
          putMVar lock (either (const Nothing) Just outcome)
          either (throwIO :: SomeException -> IO ()) (const (pure ())) outcome
      lookupInstance element = do
        key <- js_instance_id element
        IntMap.lookup key <$> readIORef instances
  onConnect <- mkSyncCallback mountElement
  -- Whatever changed, the input is read afresh from the element under the
  -- lock, so that the last change to run has the last word, whichever order
  -- the threads run in.
  onChange <- mkSyncCallback $ \element ->
    lookupInstance element >>= traverse_ (\(Instance lock) -> withMVar lock (traverse_ (\live -> readProps element >>= live.setInput . definition.input)))
  onDisconnect <- mkSyncCallback $ \element -> do
    key <- js_instance_id element
    atomicModifyIORef' instances (\is -> (IntMap.delete key is, IntMap.lookup key is)) >>= traverse_ disposeInstance
  onMethod <- mkSyncCallback (callMethod definition.methods lookupInstance)
  -- The page's registry keeps how to dispose of this definition too, and
  -- calls it when another program in the page (the tutorial's app and the
  -- program it compiled, a run of browser GHCi after another) defines the
  -- name: that program cannot see this one's registry. The callback is not
  -- freed with the others, since it may be the one running then; it is
  -- freed a moment later, from a thread of its own (see disposeAll).
  disposeRef <- newIORef (pure ())
  onDispose <- mkSyncCallback (\_ -> join (readIORef disposeRef))
  let callbacks = [onConnect, onChange, onDisconnect, onMethod]
  defined <-
    js_define
      (toJSText name)
      token
      (toJSText (encodeText definition.observed))
      (toJSText (encodeText [n | Method {name = n} <- definition.methods]))
      (shadowCode definition.shadow)
      (toJSText definition.styles)
      onConnect
      onChange
      onDisconnect
      onMethod
      onDispose
  refused <- js_is_error defined
  when refused $ do
    traverse_ freeCallback (onDispose : callbacks)
    refusal <- JSError <$> (fromJSText <$> js_error_name defined) <*> (fromJSText <$> js_error_message defined)
    throwIO refusal
  disposed <- newIORef False
  let disposeAll = do
        already <- atomicModifyIORef' disposed (True,)
        unless already $ do
          js_inert (toJSText name) token
          atomicModifyIORef' registry (\r -> (Map.update (\entry -> if fst entry == token then Nothing else Just entry) name r, ()))
          atomicModifyIORef' instances (IntMap.empty,) >>= traverse_ disposeInstance
          traverse_ freeCallback callbacks
          -- Freed once whatever called this has returned: when the page's
          -- registry disposes of this definition for another program, the
          -- caller is onDispose itself, which must not go away under its
          -- own call. A thread that first waits yields to the page, so the
          -- call is over by the time it frees.
          void (forkIO (threadDelay 10000 >> freeCallback onDispose))
  writeIORef disposeRef disposeAll
  atomicModifyIORef' registry (\r -> (Map.insert name (token, disposeAll) r, ()))
  -- An element already on the page that fails to mount (its component's
  -- initialState threw, say) fails the definition: the elements mounted
  -- before it, the callbacks and the name's hooks go with it, so that a
  -- caller who got no registration has nothing left behind.
  connected <- js_length defined
  for_ [0 .. connected - 1] (js_index defined >=> mountElement) `onException` disposeAll
  pure Registration {dispose = liftIO disposeAll}

-- | The names defined in this program, each with the token of its current
-- definition and how to dispose of it: what the same program defining a
-- name again disposes of before it asks the page. (The page's registry
-- keeps a callback for the same purpose, for another program defining
-- the name.)
registry :: IORef (Map Text (Int, IO ()))
registry = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE registry #-}

-- | What the browser would say of the name, as it would say it: a name it
-- would not take is a @SyntaxError@. The browser has a few more rules (a
-- handful of names reserved by SVG and MathML, characters a name may not
-- hold), which 'define' leaves to it.
badName :: Text -> Maybe JSError
badName name
  | Just (first, _) <- Text.uncons name
  , isAsciiLower first
  , Text.any (== '-') name
  , not (Text.any isAsciiUpper name) =
      Nothing
  | otherwise =
      Just
        JSError
          { name = "SyntaxError"
          , message =
              "Halogen.CustomElement.define: "
                <> Text.pack (show name)
                <> " is not a valid custom element name: it must start with a lower-case letter, contain a hyphen and have no upper-case letters, as \"x-counter\" does"
          }

shadowCode :: Shadow -> Int
shadowCode = \case
  NoShadow -> 0
  OpenShadow -> 1
  ClosedShadow -> 2

--------------------------------------------------------------------------------
-- An element's component
--------------------------------------------------------------------------------

-- | One element's tree, behind the lock its changes are made under: empty
-- while it mounts, then the tree, or 'Nothing' once it is disposed of (or
-- if it failed to mount).
newtype Instance query input = Instance (MVar (Maybe (Live query input)))

-- | A mounted tree, as the element's callbacks use it.
data Live query input = Live
  { setInput :: input -> IO ()
  , send :: forall a. query a -> IO (Maybe a)
  , end :: IO ()
  }

-- | What the tree's root answers: a new input, which it renders the
-- component with, or a query for the component, which it hands on.
data Wrapped query input a
  = SetInput input a
  | Forward (query a)

type Inner query output = ("element" .== H.Slot query output ())

-- | The parent each element mounts: its state is the input, which it
-- renders the component with, and what the component raises it dispatches
-- on the element at once, in the action that hears it. So a new input is a
-- render of the parent, and the component hears it as @Receive@, as it
-- would under any parent; and an output is an event by the time the raise
-- returns, one raised in the component's @Initialize@ included, which a
-- subscription made after 'runUI' returned would have missed.
wrapper :: forall query input output m. (MonadIO m) => (output -> IO ()) -> H.Component query input output m -> H.Component (Wrapped query input) input output m
wrapper dispatch inner = H.component pure render [H.onAction (H.liftEffect . liftIO . dispatch), H.onQuery answer]
  where
    render :: input -> H.ComponentHTML output (Inner query output) m
    render current = HH.slot "element" () inner current id

    answer :: Wrapped query input a -> H.HalogenM input output (Inner query output) output m (Maybe a)
    answer = \case
      SetInput new reply -> Just reply <$ put new
      Forward q -> H.query "element" () q

-- | Mount the component in the element, and dispatch its outputs there.
mountOn
  :: forall query input output m
   . (MonadBrowserDOM m, MonadUnliftIO m)
  => (forall a. m a -> IO a)
  -> Definition query input output
  -> H.Component query input output m
  -> JSVal
  -> IO (Live query input)
mountOn run definition component element = do
  container <- js_mount_point element
  props <- readProps element
  let raised o = for_ (definition.output o) $ \(event, detail) ->
        js_dispatch element (toJSText event) (toJSText (encodeText detail))
  socket@H.HalogenSocket {query = ask} <- run (runUI (wrapper raised component) (definition.input props) (HTMLElement container))
  pure
    Live
      { setInput = \new -> void (run (ask (SetInput new ())))
      , send = run . ask . Forward
      , end = run socket.dispose
      }

-- | Dispose of an element's tree, once: whoever takes the lock after this
-- finds it gone.
disposeInstance :: forall query input. Instance query input -> IO ()
disposeInstance (Instance lock) = mask $ \restore -> do
  live <- takeMVar lock
  restore (traverse_ (.end) live) `onException` putMVar lock Nothing
  putMVar lock Nothing

-- | What the page has set on the element of the observed names. The page's
-- registry gives it as JSON, which cannot fail to decode.
readProps :: JSVal -> IO Props
readProps element = do
  json <- fromJSText <$> js_props element
  pure $ case eitherDecodeStrictText json of
    Right (attributes, properties) -> Props {attributes, properties}
    Left _ -> Props {attributes = Map.empty, properties = Map.empty}

--------------------------------------------------------------------------------
-- Props
--------------------------------------------------------------------------------

-- | What the page has set on an element of the names its definition
-- 'observed': attributes, which are text, as HTML writes them; and
-- properties, set from JavaScript, which are any value JSON can carry. React
-- 19, Vue, Lit and Svelte set a property rather than an attribute for
-- anything that is not a string, so an element meant for them reads both.
data Props = Props
  { attributes :: Map Text Text
  , properties :: Map Text Value
  }
  deriving stock (Eq, Show)

-- | The attribute's value, if the element has it.
attr :: Text -> Props -> Maybe Text
attr name props = Map.lookup name props.attributes

-- | The property's value, if it has been set, and decodes as the type asked
-- for. A property is the element's own: setting it does not set the
-- attribute, nor the other way round.
prop :: forall a. (FromJSON a) => Text -> Props -> Maybe a
prop name props = Map.lookup name props.properties >>= parseMaybe parseJSON

--------------------------------------------------------------------------------
-- Methods
--------------------------------------------------------------------------------

-- | An element method, made with 'method' or 'tellMethod'.
data Method query = forall args result. Method
  { name :: Text
  , decode :: Value -> Maybe args
  , ask :: args -> query result
  , answer :: result -> Value
  }

-- | A method that sends the component a request, and resolves with its
-- answer: @element.count()@ is a promise of what @GetCount@ answers, as
-- JSON, or of @null@ if the component did not answer (it gave 'Nothing', or
-- the element has no component, being disconnected or its name inert).
--
-- > CE.method "count" (\() -> GetCount)
--
-- The arguments are JSON too: none is @[]@, so a method taking @()@ is
-- called with none; one is that value; several are an array, so a method
-- of a pair is called with two. Arguments that do not decode reject the
-- promise, as does a query that fails.
method :: forall args result query. (FromJSON args, ToJSON result) => Text -> (args -> H.Request query result) -> Method query
method name request = Method {name, decode = parseMaybe parseJSON, ask = \args -> request args id, answer = toJSON}

-- | A method that tells the component something, and resolves with @null@
-- once it has been told.
--
-- > CE.tellMethod "reset" (\() -> Reset)
tellMethod :: forall args query. (FromJSON args) => Text -> (args -> H.Tell query) -> Method query
tellMethod name tell = Method {name, decode = parseMaybe parseJSON, ask = \args -> tell args (), answer = const Null}

-- | Answer a call of one of the element's methods. The call waits, behind
-- the lock, for the changes made before it to be applied, so that a page
-- that sets an attribute and then calls a method has its method answered
-- with the new input; the query itself is sent outside the lock, so that a
-- method that takes a while holds up no change.
callMethod :: forall query input. [Method query] -> (JSVal -> IO (Maybe (Instance query input))) -> JSVal -> IO ()
callMethod methods lookupInstance call = do
  element <- js_call_element call
  name <- fromJSText <$> js_call_name call
  args <- fromJSText <$> js_call_args call
  outcome <- try $ case [m | m@Method {name = n} <- methods, n == name] of
    [] -> pure "null"
    Method {decode, ask, answer} : _ -> case eitherDecodeStrictText args >>= maybe (Left "") Right . decode of
      Left _ -> throwIO (userError (Text.unpack ("the arguments of " <> name <> " do not decode: " <> args)))
      Right decoded ->
        lookupInstance element >>= \case
          Nothing -> pure "null"
          Just (Instance lock) ->
            withMVar lock pure >>= \case
              Nothing -> pure "null"
              Just Live {send} -> maybe "null" (encodeText . answer) <$> send (ask decoded)
  case outcome of
    Right json -> js_resolve call (toJSText json)
    Left (failure :: SomeException) -> js_reject call (toJSText (Text.pack (displayException failure)))

--------------------------------------------------------------------------------
-- Others' elements
--------------------------------------------------------------------------------

-- | A handler for an event with a @detail@, such as the @CustomEvent@s a
-- custom element dispatches, given that detail decoded from JSON. An event
-- whose detail does not decode as the type asked for (or that has none, for
-- a type that @null@ does not decode as) makes no action.
--
-- > HH.element (HH.ElemName "x-colour-picker")
-- >   [HP.attr (HH.AttrName "value") st.colour, CE.onEvent "colour-changed" Picked]
-- >   []
--
-- An element made elsewhere is used as any other: 'HH.element' with its
-- name, 'Halogen.HTML.Properties.attr' for its attributes and
-- 'Halogen.HTML.Properties.prop' for its properties.
onEvent :: forall detail action r. (FromJSON detail) => Text -> (detail -> Maybe action) -> IProp r action
onEvent name f = HE.handler' (EventType name) (\(Event event) -> either (const Nothing) f (eitherDecodeStrictText (fromJSText (js_detail event))))

--------------------------------------------------------------------------------

encodeText :: forall a. (ToJSON a) => a -> Text
encodeText = decodeUtf8 . Lazy.toStrict . encode
