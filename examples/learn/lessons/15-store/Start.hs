{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lesson (Store (..), Action (..), initialStore, reduce, component) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (modify)
import Data.Row (Empty, type (.+), type (.==))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Store qualified as Store

-- The application's state, in one place, and what can happen to it.
data Store = Store {count :: Int, name :: Text}

data Action = Increment | Rename Text

initialStore :: Store
initialStore = Store {count = 0, name = "Ada"}

reduce :: Store -> Action -> Store
reduce store Increment = store {count = store.count + 1}
reduce store (Rename new) = store {name = new}

-- Shows the part of the store it is connected to, and how many times it
-- was rendered again because that part changed. Its state holds a value of
-- the type a, and a component's state must be Typeable (Halogen's trace
-- names a component after its state's type), hence the constraint.
display :: forall a q o m. (Typeable a, MonadIO m) => Text -> (a -> Text) -> H.Component q (Store.Connected a ()) o m
display label toText =
  H.component initialState render [H.onReceive receive]
  where
    initialState connected = pure (connected.context, 0 :: Int)
    render :: (a, Int) -> H.ComponentHTML () Empty m
    render (value, again) =
      HH.p_ [HH.text (label <> ": " <> toText value <> " (rendered again: " <> T.pack (show again) <> ")")]
    receive connected = modify (\(_, again) -> (connected.context, again + 1))

countView :: forall q o m. (Store.MonadStore Action Store m) => H.Component q () o m
countView = Store.connect (Store.selectEq (.count)) (display "Count" (T.pack . show))

-- Not connected yet: it shows the name the store starts with, and never
-- hears of another.
nameView :: forall q o m. (MonadIO m) => H.Component q () o m
nameView =
  H.component (\_ -> pure ()) render []
  where
    render :: () -> H.ComponentHTML () Empty m
    render _ = HH.p_ [HH.text ("Name: " <> initialStore.name <> " (not connected)")]

data ToolbarAction = Add | NewName Text

-- Changes the store, and keeps nothing of its own.
toolbar :: forall q o m. (Store.MonadStore Action Store m) => H.Component q () o m
toolbar =
  H.component (\_ -> pure ()) render [H.onAction handleAction]
  where
    render :: () -> H.ComponentHTML ToolbarAction Empty m
    render _ =
      HH.div_
        [ HH.button [HE.onClick (\_ -> Add)] [HH.text "Add one"]
        , HH.input [HP.placeholder "A new name", HE.onValueInput NewName]
        ]
    handleAction Add = Store.updateStore Increment
    handleAction (NewName new) = Store.updateStore (Rename new)

type Slots =
  ( "toolbar"
      .== H.Slot H.VoidF Void ()
      .+ "count"
      .== H.Slot H.VoidF Void ()
      .+ "name"
      .== H.Slot H.VoidF Void ()
  )

component :: forall q o m. (Store.MonadStore Action Store m) => H.Component q () o m
component =
  H.component (\_ -> pure ()) render []
  where
    render :: () -> H.ComponentHTML () Slots m
    render _ =
      HH.div_
        [ HH.slot_ "toolbar" () toolbar ()
        , HH.slot_ "count" () countView ()
        , HH.slot_ "name" () nameView ()
        ]
