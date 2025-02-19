module Main where

import Data.ByteString qualified as BS
import Data.IORef
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTPTypes
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.Test
import Test.Hspec
import Test.Hspec.Wai ( get, shouldRespondWith, with )
import Type.Reflection qualified as Network.Wai

app req respond =
  respond $
    Wai.responseLBS HTTPTypes.status200 [] "Hello, World!"

main = hspec spec

spec = with (return app) $ do
  describe "E2E Request Handling" $
    it "returns a 200 OK response for the root path" $ do
      get "/" `shouldRespondWith` 200

-- let request =
--       WaiTest.defaultRequest
--         { WaiTest.requestMethod = HTTP.methodGet,
--           WaiTest.pathInfo = []
--         }
-- let session = WaiTest.request request
-- response <- WaiTest.runSession session app
-- WaiTest.assertStatus 200 response
-- WaiTest.assertBody (Char8.pack "Hello, World!") response