module Server.SSE
  ( createSSEStream
  , sendSSEMessage
  , startPingInterval
  ) where

import Prelude

import Effect (Effect)
import Node.Stream (Readable)

-- Foreign imports from SSE.js
foreign import createSSEStream :: forall r. Effect (Readable r)

foreign import sendSSEMessage :: forall r. Readable r -> String -> String -> Effect Unit

foreign import startPingInterval :: forall r. Readable r -> Int -> Effect (Effect Unit)