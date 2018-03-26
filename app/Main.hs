{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import           Control.Exception
import           EventBriteAPI
import           Server
import           System.Cron.Schedule
import           System.Environment      (getEnv)

main :: IO ()
main = do
    conf <- EventBritConf <$> getEnv "ORGANIZER_TOKEN" <*> getEnv "EVENTBRITE_TOKEN"
    cache <- newMVar []
    loadJob conf cache
    execSchedule $ addJob (loadJob conf cache) "0 * * * *"
    startServer cache
    where
        loadJob :: EventBritConf -> MVar Cache -> IO ()
        loadJob conf cache = catchAny load print
            where
                catchAny :: IO a -> (SomeException -> IO a) -> IO a
                catchAny = Control.Exception.catch

                load :: IO ()
                load = do
                    putStrLn "reload cache."
                    users <- loadAttendees conf
                    swapMVar cache users
                    return ()
