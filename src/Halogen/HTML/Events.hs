module Halogen.HTML.Events where

import Data.Row
import HPrelude
import Halogen.HTML.Core qualified as Core
import Halogen.HTML.Properties
import Halogen.Query.Input
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event
import Web.UIEvent.MouseEvent
import Web.UIEvent.MouseEvent.EventTypes qualified as MET

handler :: forall r i. EventType -> (Event -> i) -> IProp r i
handler et f = IProp $ Core.handler et (Just . Action . f)

mouseHandler :: forall i. (MouseEvent -> i) -> Event -> i
mouseHandler = unsafeCoerce

onClick :: forall r i. (HasType "onClick" MouseEvent r) => (MouseEvent -> i) -> IProp r i
onClick = handler MET.click . mouseHandler
