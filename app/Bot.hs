module Bot
 ( botRun
 , botPost
 )
where

import Web.Hastodon
import Data.Text (unpack, strip, pack)
import Control.Concurrent (threadDelay)
-- TODO should wake up twice a minute, log postings to avoid double post

strip' :: String -> String
strip' = unpack.strip.pack

login :: IO (Maybe HastodonClient)
login = do
  key         <- strip' <$> readFile "key"
  secret      <- strip' <$> readFile "secret"
  email       <- strip' <$> readFile "email"
  password    <- strip' <$> readFile "password"
  server      <- strip' <$> readFile "server"
  mkHastodonClient key secret email password server

botPost :: HastodonClient -> String -> String -> IO Bool
botPost client cw txt = do
  let opt = spoilerText cw
  posted <- postStatusWithOption client opt txt
  case posted of
    Left  _      -> return False
    Right status -> return True


-- | initiate bot operation. this is the "main" function
botRun :: IO ()
botRun = do
  maybeClient <- login
  case maybeClient of
    Nothing     -> putStrLn "login failed! is strip' fucking your password?"
    Just client -> do
      return ()
