-- | The suite that runs under Node on every backend (and natively also
-- serves a component over a real WebSocket).
module Main (main) where

import Protolude
import Test.Hspec (hspec)
import Test.Operations qualified
import Test.Page qualified
import Test.Probe qualified
import Test.Protocol qualified
import Test.Recorder qualified
import Test.Server qualified
import Test.Session qualified

main :: IO ()
main = hspec $ do
  Test.Protocol.spec
  Test.Recorder.spec
  Test.Session.spec
  Test.Operations.spec
  Test.Page.spec
  Test.Server.spec
  Test.Probe.spec
