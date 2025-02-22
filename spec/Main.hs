{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception qualified as Wai
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTPTypes
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.Test
import Test.Hspec
import Test.Hspec.Wai (get, shouldRespondWith, with)
import Type.Reflection qualified as Network.Wai

data RouteTree = RouteTree
  { treeChildren :: [RouteNode]
  }

data RouteNode = RouteNode
  { nodeChildren :: [RouteNode],
    handler :: Wai.Application,
    path :: String
  }

data RouteBuilder = RouteBuilder {resources :: String -> RouteOptions -> IO ()}

data RouteOptions = RouteOptions
  { only :: Actions,
    except :: Actions
  }
  deriving (Show)

defaultRouteOptions =
  RouteOptions
    { only = defaultOnlyActions,
      except = defaultOnlyActions
    }

data Actions = Actions
  { showAction :: Bool,
    indexAction :: Bool,
    newAction :: Bool,
    createAction :: Bool,
    editAction :: Bool,
    updateAction :: Bool,
    destroyAction :: Bool
  }
  deriving (Show)

defaultOnlyActions =
  Actions
    { showAction = False,
      indexAction = False,
      newAction = False,
      createAction = False,
      editAction = False,
      updateAction = False,
      destroyAction = False
    }

makeRoutes routingCallback = do
  routeTreeRef <- newIORef RouteTree {treeChildren = []}
  let resources name options = do
        liftIO $ putStrLn $ "Creating resources for " <> name <> " with options: " <> show options
        currentRouteTree <- readIORef routeTreeRef
        let newRouteTree =
              RouteTree
                { treeChildren =
                    treeChildren currentRouteTree
                      <> [ RouteNode
                             { nodeChildren = [],
                               handler = \_req respond -> respond $ Wai.responseLBS HTTPTypes.status200 [] "Hello, orld!",
                               path = name
                             }
                         ]
                }
        writeIORef routeTreeRef newRouteTree
  let builder = RouteBuilder {resources = resources}
  routingCallback builder
  currentRouteTree <- readIORef routeTreeRef
  return currentRouteTree

routes = makeRoutes $ \(RouteBuilder {..}) -> do
  resources "posts" defaultRouteOptions {only = defaultOnlyActions {showAction = True}}

app req respond = liftIO $ do
  putStrLn $ "Request received: " <> show (Wai.pathInfo req)
  routeTree <- routes
  case (treeChildren routeTree) of
    [] -> respond $ Wai.responseLBS HTTPTypes.status404 [] "No routes found"
    (firstRoute : _) -> (handler firstRoute) req respond

main = hspec spec

spec = with (pure app) $ do
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