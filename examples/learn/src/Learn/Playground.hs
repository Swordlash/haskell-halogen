-- | The playground: the tutorial's compiler with no lesson around it, at
-- @\/playground\/@ ("Learn.Route"). The reader picks the rootfs layers their
-- code needs, and the page passes GHC what a lesson's header would: each
-- layer's package database, and a @-package@ for each of its packages. What
-- the page keeps of it in the browser, and what a lesson hands it when the
-- reader opens the lesson's code there, are here too, so that they are read
-- the same way they are written.
module Learn.Playground
  ( template
  , codeKey
  , layersKey
  , argsKey
  , handoffKey
  , Handoff (..)
  , encodeHandoff
  , decodeHandoff
  , fromLesson
  , packageArgs
  , withoutPackages
  , readLayers
  , showLayers
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
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

-- | The key, in @sessionStorage@, of what a lesson hands the playground.
handoffKey :: Text
handoffKey = "learn:playground:handoff"

-- | A lesson's code, as the reader had it, and what compiling it took: the
-- lesson's layers, and its GHC arguments.
data Handoff = Handoff
  { code :: Text
  , layers :: [Text]
  , args :: [Text]
  }
  deriving stock (Eq, Show)

instance ToJSON Handoff where
  toJSON h = object ["code" .= h.code, "layers" .= h.layers, "args" .= h.args]

instance FromJSON Handoff where
  parseJSON = withObject "Handoff" $ \o -> Handoff <$> o .: "code" <*> o .: "layers" <*> o .: "args"

-- | A hand-off as it is kept, JSON.
encodeHandoff :: Handoff -> Text
encodeHandoff = decodeUtf8 . toS . Aeson.encode

-- | A hand-off kept, unless what is under its key is something else.
decodeHandoff :: Text -> Maybe Handoff
decodeHandoff = Aeson.decodeStrict . encodeUtf8

-- | The playground's layers and extra arguments for a hand-off: the
-- lesson's layers, and its arguments but for its @-package@ flags, which
-- name its layers' packages (a lesson's header must, see
-- @examples\/learn\/README.md@) and which the playground passes for the
-- layers checked.
fromLesson :: Handoff -> ([Text], Text)
fromLesson h = (h.layers, T.unwords (withoutPackages h.args))

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
