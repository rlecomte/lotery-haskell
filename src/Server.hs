{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Server(
    startServer
) where

import           Control.Concurrent.MVar     (MVar, readMVar)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Maybe                  (fromMaybe)
import           EventBriteAPI
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant

type WinnerAPI = "winners" :> QueryParam "nb" Int :> Get '[JSON] [User]

startServer :: MVar Cache -> IO ()
startServer cache = do
    putStrLn "Start web server"
    run 9000 $ simpleCors $ app cache

app :: MVar Cache -> Application
app cache = serve api (server cache)

api :: Proxy WinnerAPI
api = Proxy

server :: MVar Cache -> Server WinnerAPI
server cache nbWinners = liftIO $ do
    users <- readMVar cache
    winners users (fromMaybe 1 nbWinners)
