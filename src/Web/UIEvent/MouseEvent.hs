module Web.UIEvent.MouseEvent where
import Data.Foreign (Foreign)

newtype MouseEvent = MouseEvent (Foreign MouseEvent)