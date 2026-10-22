module Halogen.IO.Util where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text qualified as T
import HPrelude
import Halogen.VDom.DOM.Monad
import Numeric (showHex)
import Web.DOM.Internal.Types
import Web.DOM.ParentNode
import Web.HTML.Event.EventTypes qualified as ET
import Web.HTML.HTMLDocument.ReadyState

-- | Waits for the document to load.
awaitLoad :: (MonadBrowserDOM m, MonadIO m) => m ()
awaitLoad = do
  rs <- readyState =<< document =<< window
  mvar <- liftIO newEmptyMVar
  case rs of
    Loading -> do
      et <- windowToEventTarget =<< window
      listener <- mkEventListener (const $ liftIO $ putMVar mvar ())
      addEventListener ET.domcontentloaded listener et
      liftIO $ takeMVar mvar
      removeEventListener ET.domcontentloaded listener et
    _ -> pass

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: (MonadBrowserDOM m, MonadIO m) => m HTMLElement
awaitBody = do
  awaitLoad
  body <- selectElement (QuerySelector "body")
  maybe (panic "Could not find body") pure body

-- | The text of the element with this id in the document: what a page
-- carries for its program in a @\<script type=\"application/json\">@,
-- such as the one 'Halogen.Static.embed' writes. 'Nothing' when the page
-- has no element with the id.
--
-- The text is the element's as the page holds it, so what 'embed' escaped
-- comes back escaped, as JSON escapes, which the application's JSON parser
-- undoes. A component's 'Halogen.Component.initialState' runs in its monad
-- and can read it there.
readEmbedded :: (MonadBrowserDOM m) => Text -> m (Maybe Text)
readEmbedded ident = do
  found <- (querySelector (QuerySelector ("#" <> selectorId)) <=< documentToNode <=< document) =<< window
  for found $ \element -> texts =<< firstChild =<< elementToNode element
  where
    -- The parser makes one text of a script's, but the element may have
    -- been built otherwise.
    texts = \case
      Nothing -> pure ""
      Just node -> do
        here <-
          nodeKind node <&> \case
            TextKind txt -> txt
            _ -> ""
        (here <>) <$> (texts =<< nextSibling node)

    -- The id as it is written after @#@ in a selector, as CSS's
    -- @CSS.escape@ writes it: a character that is not a letter, a digit, a
    -- hyphen, an underscore or past ASCII is escaped, as is a digit that
    -- starts it or follows a hyphen that does, and a hyphen alone. Natively
    -- the in-memory document's selectors compare the id as written, so
    -- there only an id that needs no escaping is found.
    selectorId
      | ident == "-" = "\\-"
      | otherwise = T.concat (zipWith escape [0 :: Int ..] (T.unpack ident))
    leadingHyphen = T.take 1 ident == "-"
    escape ix c
      | isDigit c && (ix == 0 || (ix == 1 && leadingHyphen)) = "\\" <> T.pack (showHex (ord c) " ")
      | isAsciiUpper c || isAsciiLower c || isDigit c || c == '-' || c == '_' || ord c >= 0x80 = T.singleton c
      | otherwise = "\\" <> T.singleton c

-- | Tries to find an element in the document.
selectElement :: (MonadBrowserDOM m) => QuerySelector -> m (Maybe HTMLElement)
selectElement query = do
  mel <- (querySelector query <=< documentToNode <=< document) =<< window
  pure $ fromElement =<< mel
