-- | The board served to every page that opens it, from one native program:
-- each page is a session of its own (Halogen.Remote.Server), and all of
-- them show the one board, so a line posted in one tab shows up in every
-- other. Run it from the repository's root, where the page's path is:
--
-- > cabal run halogen-example-live-server --builddir=dist-newstyle/native
--
-- and open http://localhost:8081/ in two tabs.
module Main (main) where

import Example.Live (board, newBoard)
import Halogen.Remote.Server qualified as Server
import Protolude
import System.IO (BufferMode (..), hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  shared <- newBoard
  let settings = Server.defaultSettings {Server.pages = [("/", "examples/live/web/live.html", "text/html; charset=utf-8")]}
  putText ("The board is at http://localhost:" <> show settings.port <> "/")
  Server.serve settings (board shared) ()
