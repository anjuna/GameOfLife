module Main where

import Life
import Text.Printf
import Data.Time.Clock
import System.Environment(getArgs)

getIntArg :: IO Int
getIntArg = fmap (read . head) getArgs

main = do
    steps <- getIntArg
    t0 <- getCurrentTime
    printTimeSince t0
    print $ (take steps haveAGoAtLife) !! (steps - 1)
    printTimeSince t0


printTimeSince t0 = do
    t1 <- getCurrentTime
    printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)