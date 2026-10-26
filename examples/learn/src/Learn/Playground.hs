-- | The playground: the tutorial's compiler with no lesson around it, at
-- @\/playground\/@ ("Learn.Route"). The reader picks the rootfs layers their
-- code needs, and the page passes GHC what a lesson's header would: each
-- layer's package database, and a @-package@ for each of its packages. What
-- the page keeps of it in the browser, and what a lesson hands it when the
-- reader opens the lesson's code there, are here too, with what the
-- playground opens with of the two ('opening').
module Learn.Playground
  ( template
  , codeKey
  , layersKey
  , argsKey
  , Handoff (..)
  , Kept (..)
  , Opening (..)
  , opening
  , packageArgs
  , withoutPackages
  , readLayers
  , showLayers
  )
where

import Data.Text qualified as T
import Protolude

-- | What the editor opens with, and what Reset puts back: a module @Lesson@
-- exporting @component@, which the page mounts as a lesson's default
-- wrapper does ("Learn.Lesson.defaultMain").
template :: Text
template =
  T.unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , "-- The page mounts Lesson.component, with no input, into the result:"
    , "-- component :: forall q o m. (MonadIO m) => H.Component q () o m"
    , "module Lesson (component) where"
    , ""
    , "import Control.Monad.IO.Class (MonadIO)"
    , "import Control.Monad.State (modify)"
    , "import Data.Text qualified as T"
    , "import Halogen qualified as H"
    , "import Halogen.HTML qualified as HH"
    , "import Halogen.HTML.Events qualified as HE"
    , ""
    , "data Action = Clicked"
    , ""
    , "component :: forall q o m. (MonadIO m) => H.Component q () o m"
    , "component ="
    , "  H.component initialState render [H.onAction handleAction]"
    , "  where"
    , "    initialState _ = pure (0 :: Int)"
    , "    render clicks ="
    , "      HH.div_"
    , "        [ HH.button [HE.onClick (\\_ -> Clicked)] [HH.text \"Click me\"]"
    , "        , HH.p_ [HH.text (\"Clicks: \" <> T.pack (show clicks))]"
    , "        ]"
    , "    handleAction action = case action of"
    , "      Clicked -> modify (+ 1)"
    ]

-- | The keys of what the page keeps in @localStorage@: the reader's code
-- (none once it is the template again), the layers checked, and the extra
-- GHC arguments.
codeKey, layersKey, argsKey :: Text
codeKey = "learn:playground:code"
layersKey = "learn:playground:layers"
argsKey = "learn:playground:args"

-- | A lesson's code, as the reader had it, and what compiling it took: the
-- lesson's layers, and its GHC arguments. The app holds it from the
-- lesson's Open in playground to the playground's arrival, in its own state
-- rather than the browser's storage, which may refuse to keep anything.
data Handoff = Handoff
  { code :: Text
  , layers :: [Text]
  , args :: [Text]
  }
  deriving stock (Eq, Show)

-- | What @localStorage@ had under 'codeKey', 'layersKey' and 'argsKey':
-- nothing for an entry the page never kept, or that the browser would not
-- read.
data Kept = Kept
  { code :: Maybe Text
  , layers :: Maybe Text
  , args :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | What the playground opens with: the editor's code ('Nothing' for the
-- template), the layers checked and the extra GHC arguments.
data Opening = Opening
  { code :: Maybe Text
  , layers :: [Text]
  , extra :: Text
  }
  deriving stock (Eq, Show)

-- | A hand-off, if a lesson has just made one, over what was kept: the
-- lesson's code, its layers, and its arguments but for its @-package@
-- flags, which name its layers' packages (a lesson's header must, see
-- @examples\/learn\/README.md@) and which the playground passes for the
-- layers checked. The hand-off is used as it is, never read back from the
-- storage it is then written to, so that a write the browser refuses (the
-- store full, or denied) cannot open the playground with what it kept
-- before.
opening :: Maybe Handoff -> Kept -> Opening
opening handed kept = case handed of
  Just h -> Opening {code = Just h.code, layers = h.layers, extra = T.unwords (withoutPackages h.args)}
  Nothing -> Opening {code = kept.code, layers = maybe [] readLayers kept.layers, extra = fromMaybe "" kept.args}

-- | The @-package@ flags for these layers: each package of each one, by
-- the manifest (a layer's name and its packages), in order and once, but for
-- the base layer's, which every compile names already.
packageArgs :: [Text] -> [(Text, [Text])] -> [Text] -> [Text]
packageArgs base manifest checked =
  concat [["-package", p] | p <- ordNub (concat [packages | (name, packages) <- manifest, name `elem` checked]), p `notElem` base]

-- | GHC arguments without their @-package@ flags.
withoutPackages :: [Text] -> [Text]
withoutPackages = \case
  "-package" : _ : rest -> withoutPackages rest
  a : rest -> a : withoutPackages rest
  [] -> []

-- | The layers checked, as kept: their names, separated by commas.
readLayers :: Text -> [Text]
readLayers = filter (not . T.null) . map T.strip . T.splitOn ","

showLayers :: [Text] -> Text
showLayers = T.intercalate ","
