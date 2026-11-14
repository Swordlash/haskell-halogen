-- | One handler on a container for the links inside it, as a page of
-- rendered Markdown has: declared as a plain click on a link inside
-- ('HE.within' and 'HE.plainLeftClick'), its default prevented, and reading
-- the link itself in the listener as an application's handler does, from
-- the event's target ('Element.closestNow', 'Element.getAttributeNow'),
-- with a guard on whether anything prevented the click already
-- ('Event.defaultPreventedNow'). Around it, the cases it must leave to the
-- browser: a link to another tab, text that is no link, a container inside
-- a link (whose link is outside the listening element), and a container
-- whose selector does not parse (@a[href]]@: Chromium closes an attribute
-- left open at the end, so @a[href@ parses).
--
-- And handlers for what a paste or a drag carries ('HE.carrying'): a paste
-- or a drop of files is the component's (its default prevented, the files'
-- names, types and sizes shown), one of text the browser's.
module Example.Delegated (component) where

import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (State)
import Web.Clipboard.ClipboardEvent qualified as CE
import Web.DOM.Element qualified as Element
import Web.Event.Event (Event, EventType (..))
import Web.Event.Event qualified as Event
import Web.File.File qualified as File
import Web.File.FileList qualified as FileList
import Web.HTML.Event.DataTransfer (DataTransfer)
import Web.HTML.Event.DragEvent qualified as DE

data State = State {followed :: [Text], pasted :: Text, dropped :: Text, dragged :: Int}

data Action = Followed Text | Pasted Text | Dropped Text | DraggedOver

component :: forall q i o m. (Monad m) => H.Component q i o m
component =
  H.component (const (pure (State [] "" "" 0))) render [H.onAction handleAction]
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      HH.div_
        [ HH.div
            [HP.class_ (HH.ClassName "delegated-links"), followingLinks "a[href]"]
            [ HH.p_ [HH.a [HP.class_ (HH.ClassName "delegated-one"), HP.href "#delegated-one"] [HH.span [HP.class_ (HH.ClassName "delegated-inner")] [HH.text "one"]]]
            , HH.p_ [HH.a [HP.class_ (HH.ClassName "delegated-two"), HP.href "#delegated-two"] [HH.text "two"]]
            , HH.p_ [HH.a [HP.class_ (HH.ClassName "delegated-blank"), HP.href "#delegated-blank", HP.target "_blank"] [HH.text "blank"]]
            , HH.p [HP.class_ (HH.ClassName "delegated-text")] [HH.text "no link"]
            ]
        , HH.a
            [HP.class_ (HH.ClassName "delegated-outer"), HP.href "#delegated-outer"]
            [HH.span [HP.class_ (HH.ClassName "delegated-inside"), followingLinks "a[href]"] [HH.text "inside a link"]]
        , HH.div
            [HP.class_ (HH.ClassName "delegated-broken"), followingLinks "a[href]]"]
            [HH.a [HP.class_ (HH.ClassName "delegated-broken-link"), HP.href "#delegated-broken"] [HH.text "broken"]]
        , HH.p [HP.class_ (HH.ClassName "delegated-followed")] [HH.text (T.unwords st.followed)]
        , HH.div
            [ HP.class_ (HH.ClassName "delegated-paste")
            , HE.preventingDefault (HE.matching (HE.carrying ["Files"]) (HE.onPaste (Pasted . describe . CE.clipboardData)))
            , HE.preventingDefault (HE.matching (HE.carrying ["Files"]) (HE.onDragOver (const DraggedOver)))
            , HE.preventingDefault (HE.matching (HE.carrying ["Files"]) (HE.onDrop (Dropped . describe . DE.dataTransfer)))
            ]
            [HH.textarea [HP.class_ (HH.ClassName "delegated-area")]]
        , HH.p [HP.class_ (HH.ClassName "delegated-pasted")] [HH.text st.pasted]
        , HH.p [HP.class_ (HH.ClassName "delegated-dropped")] [HH.text st.dropped]
        , HH.p [HP.class_ (HH.ClassName "delegated-dragged")] [HH.text (show st.dragged)]
        ]

    followingLinks selector =
      HE.preventingDefault (HE.matching (HE.within selector HE.plainLeftClick) (HE.handler' (EventType "click") (fmap Followed . clickedLink selector)))

    handleAction :: Action -> H.HalogenM State Action Empty o m ()
    handleAction = \case
      Followed href -> modify $ \s -> s {followed = s.followed <> [href]}
      Pasted files -> modify $ \s -> s {pasted = files}
      Dropped files -> modify $ \s -> s {dropped = files}
      DraggedOver -> modify $ \s -> s {dragged = s.dragged + 1}

-- | The link clicked, read in the listener: nothing prevented the click
-- yet (the handler's own 'HE.preventingDefault' comes after it decides),
-- and the link around the target has an address.
clickedLink :: Text -> Event -> Maybe Text
clickedLink selector event = do
  guard (not (Event.defaultPreventedNow event))
  target <- Event.target event >>= Element.fromEventTarget
  anchor <- Element.closestNow target selector
  Element.getAttributeNow anchor "href"

-- | The files a transfer carries, read in the listener.
describe :: Maybe DataTransfer -> Text
describe = maybe "none" $ \transfer ->
  T.intercalate ";" [File.name f <> "," <> File.type_ f <> "," <> show (File.size f) | f <- FileList.now transfer.files]
