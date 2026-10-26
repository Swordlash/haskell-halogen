{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the demo library hands over, as the program that loads it names
-- it: a component greeting the name it is given, which waves when its
-- button is pressed or when it is told to, and says how often it has.
--
-- The component itself is in the shared library (@feature/Feature.hs@,
-- built into @liblazy-demo.so@), which a program loads with
-- "Halogen.Lazy.Dyld" and shows with "Halogen.Lazy":
--
-- > Dyld.load (Dyld.library "rootfs/layers/lazy/liblazy-demo.so" Demo.entry) :: IO (Demo.Feature BrowserDOM)
module Halogen.Lazy.Demo
  ( Feature
  , Query (..)
  , Output (..)
  , entry
  )
where

import Data.Text (Text)
import Halogen qualified as H

-- | The component: its input is the name it greets.
type Feature m = H.Component Query Text Output m

-- | What a parent can ask of it: to wave, and how many times it has.
data Query a
  = Wave a
  | GetWaves (Int -> a)

-- | What it raises each time it waves: how many times it has, in all.
newtype Output = Waved Int
  deriving stock (Eq, Show)

-- | The name of the function the library exports, which hands the
-- component over.
entry :: Text
entry = "halogen_lazy_demo"
