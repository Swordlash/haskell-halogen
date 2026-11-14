{-# LANGUAGE TemplateHaskell #-}

-- | The program's bindings to the client (jsbits/remote.js), for the two
-- things a program in a page does with it: parse an event's JSON into the
-- object a handler reads, and run a client in the page itself
-- ("Halogen.Remote.Loopback"), whose first message is this page's hello. Natively each is an inert stub, and nothing
-- calls them: an event is read from the aeson value there.
module Halogen.Remote.Client
  ( parse
  , attach
  , apply
  , detach
  , hello
  )
where

import Halogen.JSBits (Callback, JSText, JSVal, Safety (..), browserJS, fromJSText, toJSText)
import Protolude

$( browserJS
     ["jsbits/remote.js"]
     [ ("js_parse", "halogen_remote_parse", Safe, [t|JSText -> IO JSVal|])
     , ("js_loopback", "halogen_remote_loopback", Safe, [t|JSVal -> Callback -> IO JSVal|])
     , ("js_apply", "halogen_remote_apply", Safe, [t|JSVal -> JSText -> IO ()|])
     , ("js_detach", "halogen_remote_detach", Safe, [t|JSVal -> IO ()|])
     , ("js_hello", "halogen_remote_hello_text", Safe, [t|IO JSText|])
     ]
 )

-- | JSON text as the JavaScript value it spells.
parse :: Text -> IO JSVal
parse = js_parse . toJSText

-- | A client in an element of this page, which sends the page's events to
-- the callback as JSON text.
attach :: JSVal -> Callback -> IO JSVal
attach = js_loopback

-- | Apply one message of the session's to the page.
apply :: JSVal -> Text -> IO ()
apply client = js_apply client . toJSText

-- | Stop listening, and empty the element.
detach :: JSVal -> IO ()
detach = js_detach

-- | What a client says first: where this page is and what its stores hold
-- of the library's items ("Halogen.Remote.Protocol"'s hello), as JSON text.
hello :: IO Text
hello = fromJSText <$> js_hello
