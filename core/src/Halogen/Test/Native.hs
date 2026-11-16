{-# LANGUAGE MultiWayIf #-}

-- | Test a component natively, on the in-memory DOM ("Halogen.VDom.DOM.Monad.Native").
--
-- Mount a component with 'withView'. Act on it as the reader does with
-- 'click', 'typeInto' and 'press'. Wait for the program with 'settle'. Read
-- what it shows with 'textOf', 'find' and 'html'. It is plain 'IO', so it
-- works with any test framework.
--
-- > import Halogen.Test.Native qualified as T
-- >
-- > it "saves the title" $
-- >   T.withView id editor () $ \view -> do
-- >     T.typeInto view "input.title" "Notes"
-- >     T.click view "button.save"
-- >     T.within (T.html view) (T.settle view)
-- >     T.textOf view ".status" `shouldReturn` "Saved"
--
-- Each view has a page of its own, with its own document and storage. So
-- views can run side by side on threads, and an event in one view's page
-- never reaches another's listeners.
--
-- The first argument of 'withView' turns the component's monad into
-- 'MemDOM'. Pass 'id' for a component in 'MemDOM', and
-- @flip runReaderT context@ for one in a @ReaderT@ over it.
--
-- A selector that finds nothing throws a 'Failure'. Its message names the
-- selector and shows the view's HTML.
--
-- = Holding what the program asks for
--
-- A test can answer a component's requests itself, in the order it
-- chooses. Make a 'Gate' and have the test's backend 'hold' each request
-- on it. Then wait with 'settleHolding' until the program has done all it
-- can, read what is held with 'held', and 'answer' it:
--
-- > gate <- T.newGate
-- > T.withView (flip runReaderT (backendOn gate)) list () $ \view -> do
-- >   T.settleHolding view (T.holds gate)
-- >   asks <- T.held gate
-- >   for_ (reverse asks) $ \ask ->
-- >     T.answer gate ask (resultFor ask.asked)
-- >   T.settleHolding view (T.holds gate)
--
-- This module also re-exports "Halogen.Activity.Control": 'withHeldWaits'
-- holds every wait of a program without a backend of the test's own, and
-- 'withTestClock' moves the program's pauses with a clock the test owns.
-- Mount the view inside the bracket, on the bracket's thread.
module Halogen.Test.Native
  ( -- * Mounting
    View (page, container)
  , withView
  , mountIn
  , dispose
  , query
  , request
  , tell
  , setInput
  , outputs

    -- * Acting as the reader
  , click
  , clickNode
  , clickOutside
  , typeInto
  , typeIntoNode
  , press
  , pressNode
  , focus
  , focusNode
  , blur
  , blurNode

    -- * Reading
  , find
  , findAll
  , element
  , textOf
  , html
  , property
  , checked
  , N.attribute
  , N.textContent
  , N.isConnected
  , N.performedOn
  , N.Performed (..)
  , N.NativeNode
  , N.Page
  , N.PropScalar (..)

    -- * Waiting for the program
  , settle
  , within

    -- * Holding what the program asks for
  , Gate
  , Held (..)
  , newGate
  , hold
  , held
  , answer
  , Holds
  , holds
  , settleHolding

    -- * Controlling the program
  , module Halogen.Activity.Control

    -- * Failures
  , Failure (..)
  )
where

import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, mask, onException, throwIO, try)
import Control.Monad (filterM, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (modify)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Foldable (for_, traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Row (type (.==))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Void (Void)
import Halogen qualified as H
import Halogen.Activity (Activity (..), quiet)
import Halogen.Activity.Control
import Halogen.HTML qualified as HH
import Halogen.VDom.DOM.Monad (MemDOM, MonadBrowserDOM, removeChild)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Driver (runUI)
import Halogen.VDom.Types (ElemName (..), Namespace (..))
import Prelude
import System.Timeout (timeout)
import UnliftIO (MonadUnliftIO)
import Web.DOM.Internal.Types (Node)

----------------------------------------------------------------------
-- Mounting

-- | A mounted component: its page, the container it is mounted in, and its
-- tree. Read the page with @view.page@ and the container with
-- @view.container@.
data View q i o = View
  { page :: N.Page
  -- ^ The view's page. A test can make more containers in it, or read its
  -- document ('N.pageDocument').
  , container :: N.NativeNode
  -- ^ The @div@ the component is mounted in, at the end of the page's body.
  , ask :: forall a. Driven q i a -> IO (Maybe a)
  , end :: IO ()
  , waitWith :: forall s. (Eq s) => STM s -> (Activity -> s -> Bool) -> IO ()
  , raised :: IORef [o]
  -- ^ The outputs, newest first.
  }

-- | What the test asks of the parent around the component: a query for the
-- component, or a new input for it.
data Driven q i a
  = Forward (q a)
  | Drive i a

-- | Mount a component in a new page, run the body, then dispose of the
-- view. The page is dropped with the view.
--
-- The first argument turns the component's monad into 'MemDOM': 'id' for
-- 'MemDOM' itself.
withView
  :: (MonadBrowserDOM m, MonadUnliftIO m)
  => (forall a. m a -> MemDOM a)
  -> H.Component q i o m
  -> i
  -> (View q i o -> IO b)
  -> IO b
withView run component input body = do
  page <- N.newPage
  bracket (mountIn page run component input) dispose body

-- | Mount a component in a page the test made. Use it when the test needs
-- the page first, to build something in it the component uses. Dispose of
-- the view with 'dispose'.
--
-- The component is mounted inside a parent that gives it its input, so
-- that 'setInput' works. The parent renders nothing of its own. It shows in
-- "Halogen.Trace" as a component labelled @View@, the component's parent.
mountIn
  :: forall q i o m
   . (MonadBrowserDOM m, MonadUnliftIO m)
  => N.Page
  -> (forall a. m a -> MemDOM a)
  -> H.Component q i o m
  -> i
  -> IO (View q i o)
mountIn page run component input = do
  container <- N.newContainerIn page
  raised <- newIORef []
  let inPage :: forall a. m a -> IO a
      inPage = N.runMemDOMIn page . run
  H.HalogenSocket {H.query = send, H.dispose = stop, H.waitActivityWith = wait} <-
    inPage (runUI (driven raised component) input (N.fromNative container))
  pure
    View
      { page
      , container
      , ask = inPage . send
      , end = inPage stop >> takeAway page container
      , waitWith = \extra done -> inPage (wait extra done)
      , raised
      }

-- | Take a node out of its parent.
takeAway :: N.Page -> N.NativeNode -> IO ()
takeAway page node =
  readIORef node.parentRef >>= traverse_ (\parent -> N.runMemDOMIn page (removeChild (N.fromNative node :: Node) (N.fromNative parent)))

-- | The parent around the component: it holds the input, forwards queries,
-- and notes the outputs.
driven :: forall q i o m. (MonadIO m) => IORef [o] -> H.Component q i o m -> H.Component (Driven q i) i Void m
driven raised inner =
  H.Component H.ComponentInfo {label = "View", stateType = Nothing} $
    H.componentSpec (pure . Holding 0) render
      & H.onAction (\o -> H.sync (liftIO (atomicModifyIORef' raised (\os -> (o : os, ())))))
      & H.onQuery
        ( \case
            Forward q -> H.query "view" () q
            Drive i a -> Just a <$ modify (\s -> Holding (s.given + 1) i)
        )
  where
    render :: Holding i -> H.ComponentHTML o ("view" .== H.Slot q o ()) m
    render s = HH.slot "view" () inner s.input id

-- | The parent's state: the input, and how many it has been given. The
-- count makes each new state differ from the last, so that the parent
-- renders again and the component receives even the same input.
data Holding i = Holding {given :: Int, input :: i}

-- | Dispose of the view's tree and take its container out of the page.
-- When it returns, every @Finalize@ has run its synchronous part. What a
-- finalizer waits for goes on afterwards: wait for it with 'settle'.
-- Disposing twice does nothing more.
dispose :: View q i o -> IO ()
dispose view = view.end

-- | Ask the component a query, as a parent does. 'Nothing' when it does
-- not answer.
query :: View q i o -> q a -> IO (Maybe a)
query View {ask} q = ask (Forward q)

-- | Ask the component a request query, made as 'H.mkRequest' makes it.
-- Throws a 'Failure' when it does not answer.
request :: View q i o -> H.Request q a -> IO a
request view r = query view (H.mkRequest r) >>= maybe (throwIO (Failure "request: the component did not answer the query")) pure

-- | Tell the component something, made as 'H.mkTell' makes it.
tell :: View q i o -> H.Tell q -> IO ()
tell view t = void (query view (H.mkTell t))

-- | Give the component a new input, as its parent does when it renders it
-- again. The component's @Receive@ runs, even for the same input.
setInput :: View q i o -> i -> IO ()
setInput View {ask} i = void (ask (Drive i ()))

-- | What the component has raised so far, oldest first.
outputs :: View q i o -> IO [o]
outputs view = reverse <$> readIORef view.raised

----------------------------------------------------------------------
-- Acting as the reader

-- | Click the first element in the view that the selector matches. See
-- 'clickNode' for what a click is made of.
click :: View q i o -> Text -> IO ()
click view selector = clickNode =<< matching "click" view selector

-- | Click a node, as the reader does with a mouse. Use it for a node
-- outside the view's container, such as one a portal put in the body.
--
-- The node gets these events, in order: @pointerdown@, @mousedown@,
-- @pointerup@, @mouseup@ and @click@. A disabled form control gets only the
-- two pointer events, as in a browser. So does a node inside a disabled
-- button. A listener that prevents the @pointerdown@ stops @mousedown@ and
-- @mouseup@, but not the @click@.
--
-- Then what the browser does by default, unless a @click@ listener
-- prevented it:
--
-- * A checkbox is ticked or unticked, then gets @input@ and @change@. Its
--   @indeterminate@ property, if set, becomes false.
-- * A radio button is checked and the others of its group are unchecked.
--   It gets @input@ and @change@ if it was not checked before.
-- * A submit button in a @form@ makes the form get a @submit@ event. A
--   click on a node inside the button does the same. The button is looked
--   at after the listeners have run: one that disabled it stops the submit.
--   A required field that is empty also stops it, and gets an @invalid@
--   event. Only @required@ is checked: not @pattern@, a length or a type.
--
-- The checkbox and the radio button change before the @click@ listeners
-- run, as in a browser, so a listener reads the new state. If a listener
-- prevents the default, they are put back.
--
-- Not done: following a link, a label clicking its control, a form's
-- reset, a button's @form@ attribute, and focus. The in-memory DOM has no
-- focus, so use 'focusNode' for a component that listens for it. A text
-- node is clicked through its parent element.
clickNode :: N.NativeNode -> IO ()
clickNode at = do
  target <- pointedAt at
  down <- N.dispatchOutcome "pointerdown" (pointerEvent 1) target
  -- A listener that prevents the pointerdown stops the two mouse events,
  -- as in a browser. The click still comes.
  let mouse eventType buttons = do
        off <- disabledFor target
        unless (off || down.defaultPrevented) (N.dispatch eventType (mouseEvent buttons) target)
  mouse "mousedown" 1
  N.dispatch "pointerup" (pointerEvent 0) target
  mouse "mouseup" 0
  activate target

-- | Click the body of the view's page, outside the view. A popup that
-- closes on a press outside it hears this.
clickOutside :: View q i o -> IO ()
clickOutside view =
  N.querySelector "body" (N.pageDocument view.page)
    >>= maybe (throwIO (Failure "clickOutside: the page has no body")) clickNode

-- | Dispatch the @click@ and do what it does by default.
activate :: N.NativeNode -> IO ()
activate target = do
  off <- disabledFor target
  unless off $ do
    (undo, follow) <- maybe (pure (pure (), pure ())) prepare =<< control target
    outcome <- N.dispatchOutcome "click" (mouseEvent 0) target
    if outcome.defaultPrevented then undo else follow

-- | The control a click on the node activates: the node itself, or the
-- nearest @button@ or @input@ around it. A click on an icon inside a button
-- activates the button.
control :: N.NativeNode -> IO (Maybe N.NativeNode)
control node
  | htmlTag node `elem` [Just "button", Just "input"] = pure (Just node)
  | otherwise = readIORef node.parentRef >>= maybe (pure Nothing) control

-- | Whether a click on the node goes nowhere: the node, or the control it
-- activates, is a disabled form control.
disabledFor :: N.NativeNode -> IO Bool
disabledFor node = do
  off <- N.disabledControl node
  if off then pure True else control node >>= maybe (pure False) N.disabledControl

-- | What a click does by default to the control it activates: the change
-- made before the @click@ listeners run, then how to put it back if a
-- listener prevents it, and what follows if none does.
prepare :: N.NativeNode -> IO (IO (), IO ())
prepare target = do
  let tag = htmlTag target
  kind <- inputType target
  if
    | tag == Just "input" && kind == "checkbox" -> do
        was <- checked target
        mixed <- property "indeterminate" target
        setChecked target (not was)
        -- A click also ends a checkbox's indeterminate state.
        for_ mixed (const (setProperty target "indeterminate" (N.ScalarBool False)))
        let restore = setChecked target was >> for_ mixed (setProperty target "indeterminate")
        pure (restore, changed target)
    | tag == Just "input" && kind == "radio" -> do
        group <- radioGroup target
        before <- for group (\r -> (r,) <$> checked r)
        for_ group (\r -> setChecked r (r == target))
        let was = any (\(r, c) -> r == target && c) before
        pure (for_ before (uncurry setChecked), unless was (changed target))
    | otherwise -> pure (pure (), submit target)
  where
    changed node = N.dispatch "input" (object []) node >> N.dispatch "change" (object []) node

-- | Submit the form of a submit button. It looks at the button after the
-- @click@ listeners have run, as a browser does: a listener that disabled
-- the button, changed its type or took it out of its form stops the submit.
submit :: N.NativeNode -> IO ()
submit button = do
  kind <- inputType button
  off <- N.disabledControl button
  let submits = case htmlTag button of
        Just "input" -> kind == "submit"
        -- A button with no type, or a type it does not know, submits.
        Just "button" -> kind `notElem` ["button", "reset"]
        _ -> False
  when (submits && not off) $
    formOwner button >>= traverse_ (submitForm button)

-- | Send @submit@ to a form, unless a required field is empty. Then each
-- such field gets an @invalid@ event instead, as in a browser. A form with
-- @novalidate@, or a button with @formnovalidate@, skips the check.
submitForm :: N.NativeNode -> N.NativeNode -> IO ()
submitForm button form = do
  skip <- (||) <$> has "novalidate" form <*> has "formnovalidate" button
  missing <- if skip then pure [] else filterM valueMissing =<< N.querySelectorAll "input, textarea" form
  if null missing
    then N.dispatch "submit" (object []) form
    else for_ missing (N.dispatch "invalid" (object []))
  where
    has name node = isJust <$> N.attribute name node

-- | Whether a field is required and has no value: an empty text field, an
-- unticked checkbox, or a radio button whose group has none checked. A
-- disabled or read-only field is never checked.
valueMissing :: N.NativeNode -> IO Bool
valueMissing node = do
  required <- isJust <$> N.attribute "required" node
  off <- N.disabledControl node
  readOnly <- isJust <$> N.attribute "readonly" node
  if not required || off || readOnly
    then pure False
    else do
      kind <- inputType node
      isText <- textField node
      if
        | htmlTag node == Just "input" && kind == "checkbox" -> not <$> checked node
        | htmlTag node == Just "input" && kind == "radio" -> not . or <$> (traverse checked =<< radioGroup node)
        | isText -> T.null <$> fieldValue node
        | otherwise -> pure False

-- | The text a field holds: its @value@ property, or else its @value@
-- attribute, or a @textarea@'s content.
fieldValue :: N.NativeNode -> IO Text
fieldValue node =
  property "value" node >>= \case
    Just (N.ScalarText t) -> pure t
    Just _ -> pure "set"
    Nothing
      | htmlTag node == Just "textarea" -> N.textContent node
      | otherwise -> fromMaybe "" <$> N.attribute "value" node

-- | Type into the first field in the view that the selector matches. See
-- 'typeIntoNode'.
typeInto :: View q i o -> Text -> Text -> IO ()
typeInto view selector text = do
  node <- matching "typeInto" view selector
  typeIntoNode node text

-- | Set the text of a field, as typing does: a @beforeinput@ event, then
-- the field's @value@, then an @input@ event. What the field held before is
-- replaced. If a listener prevents the @beforeinput@, the field is left as
-- it was.
--
-- A read-only field is left as it is, and gets no events, as in a
-- browser. A field that is disabled, or an element that is not a text
-- field, throws a 'Failure'.
--
-- Not done: an event for each key, the @maxlength@ limit, and @change@,
-- which a browser sends when the field loses focus. Dispatch @change@ with
-- 'N.dispatch' if the component listens for it.
typeIntoNode :: N.NativeNode -> Text -> IO ()
typeIntoNode at text = do
  field <- pointedAt at
  isField <- textField field
  unless isField $ do
    shown <- describe field
    throwIO (Failure ("typeInto: " <> shown <> " is not a text field"))
  off <- N.disabledControl field
  when off $ do
    shown <- describe field
    throwIO (Failure ("typeInto: " <> shown <> " is disabled"))
  readOnly <- isJust <$> N.attribute "readonly" field
  unless readOnly $ do
    let edit =
          object
            [ "inputType" .= (if T.null text then "deleteContentBackward" else "insertText" :: Text)
            , "data" .= (if T.null text then Aeson.Null else Aeson.String text)
            ]
    outcome <- N.dispatchOutcome "beforeinput" edit field
    unless outcome.defaultPrevented $ do
      setProperty field "value" (N.ScalarText text)
      N.dispatch "input" edit field

-- | Press a key on the first element in the view that the selector
-- matches. See 'pressNode'.
press :: View q i o -> Text -> Text -> IO ()
press view selector key = do
  node <- matching "press" view selector
  pressNode node key

-- | Press a key on a node: @keydown@, then @keyup@, with the key's name as
-- @key@ (@\"Enter\"@, @\"Escape\"@, @\"a\"@). Nothing happens by default:
-- the key types no character, and Enter submits no form.
pressNode :: N.NativeNode -> Text -> IO ()
pressNode at key = do
  target <- pointedAt at
  N.dispatch "keydown" (keyEvent key) target
  N.dispatch "keyup" (keyEvent key) target

-- | Focus the first element in the view that the selector matches. See
-- 'focusNode'.
focus :: View q i o -> Text -> IO ()
focus view selector = focusNode =<< matching "focus" view selector

-- | Dispatch @focus@, then @focusin@, on a node. The in-memory DOM does not
-- know which element has the focus, so this only tells the listeners.
focusNode :: N.NativeNode -> IO ()
focusNode at = do
  target <- pointedAt at
  N.dispatch "focus" (object []) target
  N.dispatch "focusin" (object []) target

-- | Blur the first element in the view that the selector matches. See
-- 'blurNode'.
blur :: View q i o -> Text -> IO ()
blur view selector = blurNode =<< matching "blur" view selector

-- | Dispatch @blur@, then @focusout@, on a node.
blurNode :: N.NativeNode -> IO ()
blurNode at = do
  target <- pointedAt at
  N.dispatch "blur" (object []) target
  N.dispatch "focusout" (object []) target

----------------------------------------------------------------------
-- Events

mouseFields :: Int -> Int -> [(Aeson.Key, Aeson.Value)]
mouseFields buttons detail =
  [ "button" .= (0 :: Int)
  , "buttons" .= buttons
  , "detail" .= detail
  , "clientX" .= (0 :: Int)
  , "clientY" .= (0 :: Int)
  ]
    <> modifiers

modifiers :: [(Aeson.Key, Aeson.Value)]
modifiers = ["altKey" .= False, "ctrlKey" .= False, "metaKey" .= False, "shiftKey" .= False]

-- | A mouse event: the main button, and the buttons held.
mouseEvent :: Int -> Aeson.Value
mouseEvent buttons = object (mouseFields buttons 1)

-- | A pointer event of the mouse.
pointerEvent :: Int -> Aeson.Value
pointerEvent buttons =
  object (mouseFields buttons 0 <> ["pointerId" .= (1 :: Int), "pointerType" .= ("mouse" :: Text), "isPrimary" .= True])

keyEvent :: Text -> Aeson.Value
keyEvent key = object (["key" .= key, "repeat" .= False] <> modifiers)

----------------------------------------------------------------------
-- The tree

-- | The element the reader points at: a text node's parent element.
pointedAt :: N.NativeNode -> IO N.NativeNode
pointedAt node = case node.kind of
  N.TextNode ->
    readIORef node.parentRef <&> \case
      Just parent | isElement parent -> parent
      _ -> node
  _ -> pure node
  where
    isElement n = case n.kind of
      N.ElementNode {} -> True
      _ -> False

-- | An HTML element's tag, in lower case.
htmlTag :: N.NativeNode -> Maybe Text
htmlTag node = case node.kind of
  N.ElementNode ns (ElemName tag) | maybe True (\(Namespace uri) -> uri == "http://www.w3.org/1999/xhtml") ns -> Just (T.toLower tag)
  _ -> Nothing

-- | An input's type, in lower case: @text@ when it has none.
inputType :: N.NativeNode -> IO Text
inputType node = maybe "text" T.toLower <$> N.attribute "type" node

-- | Whether typing edits the element's value.
textField :: N.NativeNode -> IO Bool
textField node = case htmlTag node of
  Just "textarea" -> pure True
  Just "input" -> (`notElem` notText) <$> inputType node
  _ -> pure False
  where
    notText = ["button", "checkbox", "color", "file", "hidden", "image", "radio", "range", "reset", "submit"]

-- | The nearest @form@ around the node.
formOwner :: N.NativeNode -> IO (Maybe N.NativeNode)
formOwner node =
  readIORef node.parentRef >>= \case
    Nothing -> pure Nothing
    Just parent
      | htmlTag parent == Just "form" -> pure (Just parent)
      | otherwise -> formOwner parent

-- | The top of the node's tree: its document, or the root of a detached tree.
rootOf :: N.NativeNode -> IO N.NativeNode
rootOf node = readIORef node.parentRef >>= maybe (pure node) rootOf

-- | The radio buttons in the node's group, the node first: the inputs of
-- type radio with the same non-empty name, the same form and the same tree.
radioGroup :: N.NativeNode -> IO [N.NativeNode]
radioGroup target = do
  name <- fromMaybe "" <$> N.attribute "name" target
  if T.null name
    then pure [target]
    else do
      root <- rootOf target
      form <- formOwner target
      inputs <- N.querySelectorAll "input" root
      let member other
            | other == target = pure False
            | otherwise = do
                kind <- inputType other
                otherName <- N.attribute "name" other
                otherForm <- formOwner other
                pure (kind == "radio" && otherName == Just name && otherForm == form)
      (target :) <$> filterM member inputs

-- | Whether a checkbox or a radio button is checked: its @checked@
-- property, or, if nothing set it, its @checked@ attribute.
checked :: N.NativeNode -> IO Bool
checked node =
  M.lookup "checked" <$> readIORef node.props >>= \case
    Just v -> pure (truthy v)
    Nothing -> isJust <$> N.attribute "checked" node
  where
    truthy = \case
      N.ScalarBool b -> b
      N.ScalarText t -> not (T.null t)
      N.ScalarInt n -> n /= 0
      N.ScalarNum d -> not (d == 0 || isNaN d)

setChecked :: N.NativeNode -> Bool -> IO ()
setChecked node b = setProperty node "checked" (N.ScalarBool b)

setProperty :: N.NativeNode -> Text -> N.PropScalar -> IO ()
setProperty node name v = atomicModifyIORef' node.props (\ps -> (M.insert name v ps, ()))

-- | A property of an element, as the component or the harness set it:
-- @value@ after 'typeInto', for instance. 'Nothing' if nothing set it.
property :: Text -> N.NativeNode -> IO (Maybe N.PropScalar)
property name node = M.lookup name <$> readIORef node.props

----------------------------------------------------------------------
-- Reading

-- | The first element in the view that the selector matches. The
-- selectors understood are "Halogen.VDom.DOM.Monad.Native.Selector"'s.
find :: View q i o -> Text -> IO (Maybe N.NativeNode)
find view selector = N.querySelector selector view.container

-- | Every element in the view that the selector matches, in document order.
findAll :: View q i o -> Text -> IO [N.NativeNode]
findAll view selector = N.querySelectorAll selector view.container

-- | The first element in the view that the selector matches. Throws a
-- 'Failure' when nothing matches.
element :: View q i o -> Text -> IO N.NativeNode
element = matching "element"

-- | The text of the first element in the view that the selector matches.
-- Throws a 'Failure' when nothing matches.
textOf :: View q i o -> Text -> IO Text
textOf view selector = N.textContent =<< matching "textOf" view selector

-- | The HTML inside the view's container, as 'N.renderToText' writes it.
html :: View q i o -> IO Text
html view = T.concat <$> (traverse N.renderToText =<< N.childNodes view.container)

-- | The first match, or a 'Failure' that names the function, the selector
-- and the view's HTML.
matching :: Text -> View q i o -> Text -> IO N.NativeNode
matching what view selector =
  find view selector >>= \case
    Just node -> pure node
    Nothing -> do
      shown <- htmlForMessage view
      throwIO
        ( Failure
            (what <> ": nothing in the view matches " <> T.pack (show selector) <> ". The view's HTML:\n" <> shown)
        )

-- | The view's HTML for a message. A tree HTML cannot write gives the
-- reason instead, so that the first failure is not hidden.
htmlForMessage :: View q i o -> IO Text
htmlForMessage view =
  try (html view) >>= \case
    Right shown -> pure shown
    Left (e :: SomeException) -> pure ("(the HTML cannot be written: " <> T.pack (show e) <> ")")

-- | A node for a message: its HTML, cut short.
describe :: N.NativeNode -> IO Text
describe node =
  try (N.renderToText node) >>= \case
    Right shown -> pure (if T.length shown > 200 then T.take 200 shown <> "…" else shown)
    Left (_ :: SomeException) -> pure "the element"

----------------------------------------------------------------------
-- Waiting for the program

-- | Wait until the view's tree is quiet: nothing running or queued, and no
-- fiber waiting in an @H.async@. Other trees in the process do not count.
-- It waits for the program, never for a clock, and has no deadline: wrap
-- it in 'within'. It does not return while the tree waits for something
-- the test holds; use 'settleHolding' then.
settle :: View q i o -> IO ()
settle View {waitWith} = waitWith (pure ()) (\a () -> quiet a)

-- | Run the action, but give up after 10 seconds: throw a 'Failure' with
-- the text the first argument gives. A guard against a test that hangs:
--
-- > T.within (("still busy:\n" <>) <$> T.html view) (T.settle view)
within :: IO Text -> IO a -> IO a
within what io =
  timeout 10_000_000 io >>= \case
    Just a -> pure a
    Nothing -> do
      shown <- what
      throwIO (Failure ("gave up after 10 seconds: " <> shown))

----------------------------------------------------------------------
-- Gates

-- | Asks of type @q@, held until the test answers each with an @r@.
data Gate q r = Gate
  { asks :: TVar (IntMap (q, TMVar r))
  , next :: TVar Int
  , changes :: TVar Word
  -- ^ How many times the asks held have changed.
  }

-- | An ask a gate holds: its number, from 1 in the order the asks came, and
-- what was asked.
data Held q r = Held
  { number :: Int
  , asked :: q
  }

-- | A gate with nothing held.
newGate :: IO (Gate q r)
newGate = Gate <$> newTVarIO IntMap.empty <*> newTVarIO 1 <*> newTVarIO 0

-- | Hold an ask until the test answers it, and return the answer. The
-- test's backend calls this from the component's @H.async@. If the waiting
-- thread is killed (its component goes away), the ask leaves the gate.
hold :: Gate q r -> q -> IO r
hold gate q = do
  slot <- newEmptyTMVarIO
  mask $ \restore -> do
    n <- atomically $ do
      n <- stateTVar gate.next (\i -> (i, i + 1))
      modifyTVar' gate.asks (IntMap.insert n (q, slot))
      modifyTVar' gate.changes (+ 1)
      pure n
    let leave = atomically $ do
          here <- IntMap.member n <$> readTVar gate.asks
          when here $ modifyTVar' gate.asks (IntMap.delete n) >> modifyTVar' gate.changes (+ 1)
    restore (atomically (takeTMVar slot)) `onException` leave

-- | The asks held now, oldest first. Call it after 'settleHolding' to see
-- every ask the program has made by then.
--
-- Asks made by different fibers at the same time can reach the gate in
-- either order. Tell them apart by 'asked', not by 'number'.
held :: Gate q r -> IO [Held q r]
held gate = map (\(number, (asked, _)) -> Held {number, asked}) . IntMap.toAscList <$> readTVarIO gate.asks

-- | Answer a held ask: the program that asked goes on with the answer.
-- Throws a 'Failure' if the ask is not held any more.
answer :: Gate q r -> Held q r -> r -> IO ()
answer gate ask r = do
  found <- atomically $ do
    m <- readTVar gate.asks
    case IntMap.lookup ask.number m of
      Nothing -> pure False
      Just (_, slot) -> do
        writeTVar gate.asks (IntMap.delete ask.number m)
        modifyTVar' gate.changes (+ 1)
        putTMVar slot r
        pure True
  unless found
    $ throwIO
    $ Failure ("answer: the ask " <> T.pack (show ask.number) <> " is not held. It was answered already, or the thread that asked is gone.")

-- | What one or more gates hold, for 'settleHolding'. Combine gates with
-- @<>@: @holds users <> holds pages@.
newtype Holds = Holds (STM (Int, Word))

instance Semigroup Holds where
  Holds a <> Holds b = Holds ((\(n, c) (m, d) -> (n + m, c + d)) <$> a <*> b)

instance Monoid Holds where
  mempty = Holds (pure (0, 0))

-- | What a gate holds.
holds :: Gate q r -> Holds
holds gate = Holds ((,) <$> (IntMap.size <$> readTVar gate.asks) <*> readTVar gate.changes)

-- | Wait until the view's tree has done all it can while the gates hold
-- their asks: nothing running or queued, and every fiber that waits is
-- waiting for a held ask. No deadline: wrap it in 'within'.
--
-- It reads the tree and the gates together, in one wait. A fiber counts
-- as waiting a moment before its ask reaches the gate, and an answered ask
-- leaves the gate a moment before its fiber stops waiting. In both cases
-- the two counts differ, so the wait goes on.
--
-- Each held ask must be the wait of one fiber of this view. An ask held
-- for another view, or two asks held by one @H.async@ at once, keep this
-- from returning.
settleHolding :: View q i o -> Holds -> IO ()
settleHolding View {waitWith} (Holds counts) =
  waitWith counts (\a (n, _) -> a.running == 0 && a.waiting == n)

----------------------------------------------------------------------
-- Failures

-- | What the harness throws when a step cannot be done: nothing matches a
-- selector, a field cannot be typed into, or a wait gave up. The message
-- says what was looked for.
newtype Failure = Failure Text

instance Show Failure where
  show (Failure message) = T.unpack message

instance Exception Failure
