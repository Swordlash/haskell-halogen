module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_, replicateM)
import Criterion.Main (bench, bgroup, defaultMain, nfIO)
import Data.IORef (writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.VDom.DOM.Monad (createTextNode, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Halogen.VDom.Types (ElemName (..))
import Prelude

-- Build the mutable DOM once, outside the measured action. Every iteration
-- reads its IORefs again and fully evaluates the resulting strict Text.
main :: IO ()
main = do
  doc <- N.newDocument
  let text value = do
        forced <- evaluate value
        N.toNative <$> runMemDOM (createTextNode forced (N.fromNative doc))
      element name attributes properties children = do
        node <- N.newElement Nothing (ElemName name)
        writeIORef node.attrs (Map.fromList [((Nothing, key), value) | (key, value) <- attributes])
        writeIORef node.props (Map.fromList properties)
        writeIORef node.kids children
        forM_ children $ \child -> writeIORef child.parentRef (Just node)
        pure node
      plain name = element name [] []
      leaf value = text value >>= \child -> plain "span" [child]
      nested depth child
        | depth == (0 :: Int) = pure child
        | otherwise = plain "div" [child] >>= nested (depth - 1)
      balanced depth
        | depth == (0 :: Int) = leaf "A leaf with <markup> & Unicode: zażółć 😀."
        | otherwise = replicateM 2 (balanced (depth - 1)) >>= plain "div"
      sized name sizes make = bgroup name <$> forM sizes (\size -> benchNode (show size) <$> make size)
      benchNode name node = bench name $ nfIO (N.renderToText node)

  small <- do
    heading <- text "Hello, Halogen!" >>= \child -> plain "h1" [child]
    body <- text "A short paragraph." >>= \child -> plain "p" [child]
    element "article" [("title", "A \"card\"")] [("className", N.ScalarText "card")] [heading, body]
  wide <- sized "wide" [100, 1000, 10000] $ \size -> do
    rows <- replicateM size $ do
      child <- text (T.replicate 8 "Some text & more. ")
      element "li" [("data-id", "123")] [("className", N.ScalarText "row")] [child]
    plain "ul" rows
  deep <- sized "deep-64KiB" [10, 100, 1000] $ \depth -> text (T.replicate 65536 "x") >>= nested depth
  trees <- sized "balanced" [7, 10] balanced
  texts <- forM
    [ ("plain-1MiB", T.replicate 1048576 "x")
    , ("escaped", T.replicate 65536 "<&>\"\r\xa0ż😀")
    ]
    $ \(name, value) -> benchNode name <$> (text value >>= \child -> plain "p" [child])
  attributes <- do
    inputs <- replicateM 1000 $ element "input" [("title", T.replicate 8 "A \"title\" <&> ż😀")] [("disabled", N.ScalarBool True)] []
    benchNode "attributes/1000" <$> plain "div" inputs
  raw <- benchNode "raw-text/1MiB" <$> (text (T.replicate 1048576 "x") >>= \child -> plain "script" [child])
  defaultMain [benchNode "small" small, wide, deep, trees, bgroup "text" texts, attributes, raw]
