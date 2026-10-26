module Main (main) where

-- Nothing from core is used, but on the JavaScript backend its polyfills
-- (core/jsbits/polyfills.js, see POLYFILLS.md) are what lets hspec start
-- under Node at all: the library needs nothing of core, its tests do.
import Data.Foreign ()
import Test.Hspec (hspec)
import Test.Run qualified as Run

main :: IO ()
main = hspec Run.spec
