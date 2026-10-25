-- | Global state for a Halogen application, a port of purescript-halogen-store.
--
-- The application has one store: a value, and a pure reducer that makes the
-- next value from it and an action. A component reads it with 'getStore',
-- changes it with 'updateStore', and follows a part of it, picked out by a
-- 'Selector', with 'connect' (its input then carries that part, and it is
-- rendered again only when that part changes) or with 'subscribe' (an action
-- for each change). The tree runs in 'StoreT', which 'runStoreT' takes away
-- at the root, before 'Halogen.VDom.Driver.runUI':
--
-- > data Store = Store {count :: Int}
-- > data Action = Increment
-- >
-- > reduce :: Store -> Action -> Store
-- > reduce store Increment = Store {count = store.count + 1}
-- >
-- > main = runBrowserDOM $ do
-- >   root <- runStoreT (Store {count = 0}) reduce App.component
-- >   void (runUI root () body)
--
-- Import it qualified (@import Halogen.Store qualified as Store@): its
-- 'subscribe' and 'select' are named as the original's are, and so the same
-- as functions of "Halogen" and "Protolude".
module Halogen.Store
  ( module Halogen.Store.Monad
  , module Halogen.Store.Select
  , module Halogen.Store.Connect
  )
where

import Halogen.Store.Connect
import Halogen.Store.Monad
import Halogen.Store.Select
