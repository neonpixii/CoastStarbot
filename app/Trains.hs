module Trains
  ( Stop
  , Train
  , TrainSet
  , trainFromFile
  ) where

import Data.Csv (FromNamedRecord, parseNamedRecord, (.:), decodeByName)
import Data.ByteString.Char8 (pack) -- cuz Data.Csv belongs in the trash.
import Data.Vector (toList)         -- THE TRASH.
import qualified Data.ByteString.Lazy.Char8 as L (pack) -- ò…ó
import Data.Time (UTCTime

data Stop = Stop { stopSn :: String -- station code
                 , stopHr :: Int    -- hour
                 , stopMn :: Int    -- minute
                 , stopCt :: String -- city
                 , stopSt :: String -- state
                 }

instance Show Stop where
  show (Stop s h m c t) = s ++ " @ " ++ (show h ++ ":" ++ show m) ++ 
                          " (" ++ c ++ ", " ++ t ++ ")"
-- Think of the train type as a "to-do" list of stations to be reached
-- As the stations are reached, they'll be removed from the Train list.

-- Train number, departure date/time, and its train.
type Train = (Int, Day, [Stop])

type TrainTime = UTCTime

instance FromNamedRecord Stop where
  parseNamedRecord r = do
    station  <- r .: (pack "Station") -- (.:) is the CSV lookup operator
    (hs, ms) <- splitAt 2 <$> r .: (pack "Time") -- string of hr and min
    city     <- r .: (pack "City")
    state    <- r .: (pack "State")
    let (hr, mn) = (read hs, read ms) :: (Int, Int)
    return $ Stop station hr mn city state

stopsFromFile :: String -> IO (Maybe [Stop])
stopsFromFile fname = do
  csv <- readFile fname
  let eitherTrain = decodeByName (L.pack csv)
  case eitherTrain of
    Left error        -> do putStrLn "Could not parse file"
                            putStrLn ("Error given: " ++ error)
                            return Nothing
    Right (_, vstops) -> do return $ Just $ toList vtrain

syncTrain :: Train -> IO Train
syncTrain (n, d, [])    = (n, d, [])
syncTrain (_, d, (first:stops)) = do
  
  let (startHr, startMn) = (stopHr first, stopMn first)
      reduce []     = []
      reduce (s:ss) = do
        now <- getCurrentTime
        let 
            then_ = UTCTime d (
            
      
