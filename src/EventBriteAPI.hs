{-# LANGUAGE OverloadedStrings #-}

module EventBriteAPI
    (
        loadAttendees,
        winners,
        Cache,
        User,
        EventBritConf(..)
    ) where

import           Control.Lens
import           Data.Aeson         as A
import           Data.Aeson.Lens    (key, nth, values, _Integer, _Value)
import           Data.Maybe         (fromJust)
import           Data.Monoid        (mconcat)
import           Data.Random
import           Data.Random.Extras as R (shuffle)
import           Data.Random.RVar   (runRVar)
import           Network.Wreq

type Cache = [User]

data User = User String String

data EventBritConf = EventBritConf {orgaId :: String, token :: String}

instance FromJSON User where
    parseJSON (Object v) =
        User  <$> v .: "first_name"
              <*> v .: "last_name"

instance ToJSON User where
    toJSON (User fn ln) =
        object ["first_name" A..= fn, "last_name" A..= ln]

loadAttendees :: EventBritConf -> IO [User]
loadAttendees EventBritConf { orgaId = orgaId, token = token } = extractId >>= getAttendees
        where
            extractId :: IO Integer
            extractId = do
                r <- get $ "https://www.eventbriteapi.com/v3/events/search/?sort_by=date&organizer.id=" ++ orgaId ++ "&token=" ++ token
                let eventId = r ^? responseBody . key "events" . nth 0 . key "id" . _Integer
                case eventId of
                    Just id -> return id
                    Nothing -> fail "No event for now."

            getAttendees :: Integer -> IO [User]
            getAttendees id = do
                r <- get urlAttendees
                let pageCount = extractPageCount r
                mconcat <$> traverse extractUser [1 .. pageCount]
                where
                    urlAttendees = "https://www.eventbriteapi.com/v3/events/" ++ show id ++ "/attendees?token=" ++ token

                    extractPageCount r = r ^?! responseBody . key "pagination" . key "page_number" . _Integer

                    extractUser :: Integer -> IO [User]
                    extractUser n = do
                        r <- get (urlAttendees ++ "&page=" ++ show n)
                        let profiles = r ^.. responseBody . key "attendees" . values . key "profile" . _Value
                        let usersResult = traverse fromJSON profiles
                        case usersResult of
                            Success users -> return users
                            Error err     -> fail err

winners :: Cache -> Int -> IO [User]
winners users nbWinners = runRVar (take nbWinners <$> R.shuffle users) StdRandom
