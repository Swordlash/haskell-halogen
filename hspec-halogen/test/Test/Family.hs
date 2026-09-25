module Test.Family (spec) where

import Example.Family qualified as Family
import Halogen qualified as H
import Data.IORef (IORef, newIORef, readIORef)
import Protolude hiding (find)
import Test.Hspec.Halogen

type Family s m = Mounted s Family.Query Family.Output m

-- | The family, and the journal its children's finalisers write to. The journal is
-- an ordinary IORef shared with the component, which only IO can read.
family :: forall m -> (MonadBrowserTest m) => PageM s (Family s m, IORef [Text])
family m = do
  journal <- unsafeIOToPageM (newIORef [])
  ui <- mount m Family.component journal
  pure (ui, journal)

addChildren :: Family s m -> Int -> PageM s ()
addChildren ui n = replicateM_ n (find ui ".add-child" >>= click)

names :: (MonadBrowserTest m) => Family s m -> PageM s (Maybe [Text])
names ui = query ui (H.mkRequest Family.GetNames)

removeChild :: Family s m -> Int -> PageM s ()
removeChild ui n = do
  children <- findAll ui ".child"
  case drop n children of
    target : _ -> findIn target ".remove" >>= click
    [] -> expectationFailure ("no child " <> show n)

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "answers a query by asking its children" $ runPage $ do
    (ui, _) <- family m
    addChildren ui 3
    names ui `shouldReturn` Just ["ada", "bo", "cy"]

  it "removes a child that asks to go, and says so" $ runPage $ do
    (ui, _) <- family m
    addChildren ui 2
    removeChild ui 0
    names ui `shouldReturn` Just ["bo"]
    outputs ui `shouldReturn` [Family.Removed "ada"]

  it "runs a removed child's finaliser" $ runPage $ do
    (ui, journal) <- family m
    addChildren ui 2
    removeChild ui 1
    eventually $ unsafeIOToPageM (readIORef journal) `shouldReturn` ["finalised bo"]

  it "runs every child's finaliser when it is unmounted" $ runPage $ do
    (ui, journal) <- family m
    addChildren ui 2
    unmount ui
    eventually $ sort <$> unsafeIOToPageM (readIORef journal) `shouldReturn` ["finalised ada", "finalised bo"]

  -- A child is initialised once it is rendered, and looks up its element by
  -- ref. Refs used to be recorded by a forked thread, which could run after
  -- the initialiser, and did when a child was added just after another was
  -- removed.
  it "lets each child find its own element when it is initialised" $ runPage $ do
    (ui, _) <- family m
    addChildren ui 2
    removeChild ui 0
    addChildren ui 1
    children <- findAll ui ".child"
    length children `shouldBe` 2
    for_ children $ \c -> findIn c ".found" >>= (`shouldHaveText` "found its element")
