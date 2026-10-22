module Halogen.Query.HalogenQ where

import Data.Functor.Coyoneda
import HPrelude

-- | What a component's @eval@ is asked to do.
data HalogenQ query msg input a
  = -- | Run once the component has rendered for the first time, after the
    -- children it rendered with have run the synchronous part of theirs.
    -- The component counts as initialized once its own has run its
    -- synchronous part: reached its first wait (a
    -- 'Halogen.Query.HalogenM.liftIO', a query that waits) or ended. Until
    -- then the outputs it raises, and those its children raise to it, are
    -- kept; then they are delivered, and from then on they arrive at once.
    -- What it waits for holds up nothing but the rest of this program: its
    -- parent's @Initialize@, the program whose render made it and the
    -- 'Halogen.IO.Driver.runUI' that mounted it go on meanwhile, and the
    -- component answers queries and handles actions as usual.
    -- (purescript-halogen waits for a component's initializer, and its
    -- children's, to end.) Whatever needs what it loads asks it with a query
    -- that waits until it is loaded.
    Initialize a
  | Finalize a
  | Receive input a
  | Action msg a
  | Query (Coyoneda query a) (() -> a)
  deriving (Functor)

instance Bifunctor (HalogenQ query msg) where
  bimap f g = \case
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Action msg a -> Action msg (g a)
    Query y g' -> Query (fmap g y) (g . g')
