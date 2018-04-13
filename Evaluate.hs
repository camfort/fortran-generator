{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Process (system)
import System.Directory (listDirectory, removeFile)
import Big.Big hiding (main)
import Control.Monad (forM_, forM, replicateM)
import Data.List (isPrefixOf, isSuffixOf, unwords)
import System.Clock
import Text.Printf
import StdError

main :: IO ()
main = do
  -- Header
  putStrLn "#funs   #funLen   #args   single   (err)     multi   (err)"
  rawData <-
   forM [2,4] $ \funArgs ->
    forM [5,10,15] $ \funs ->
     forM [5,10..20] $ \funLen -> do

      -- 1. Synthesise the whole program...
      bigSynthesise "whole" "big" funs funLen funArgs
      -- ... infer its units, recording the time
      timeWhole <- timeProcess "camfort" ["units-infer", "big.f90"]
      -- cleanup
      removeFile "big.f90"

      -- 2. Synthesis separate programs...
      bigSynthesise "separate" "big" funs funLen funArgs
      -- ... units-compile the module files
      items <- listDirectory "."
      modTimes <-
        forM (filter ("mod_" `isPrefixOf`) items) $ \modFile ->
          timeProcess "camfort" ["units-compile", modFile]
      -- Calculate the total time to compile the mod files
      let modCompileTime = sum modTimes

      -- ... infer the units for the whole file
      timeSep <- timeProcess "camfort" ["units-infer", "big.f90", "-I ."]
      -- cleanup
      removeFile "big.f90"
      forM_ (filter ("mod_" `isPrefixOf`) items) removeFile
      items <- listDirectory "."
      forM_ (filter ("fsmod" `isSuffixOf`) items) removeFile

      let (timeWhole', timeWholeSeries) = timeWhole
      let (timeSep', timeSepSeries) = timeSep + modCompileTime
      -- Report
      printf "%2d      %2d        %2d      %0.3f    (%0.3f)   %0.3f   (%0.3f)\n"
              funs     funLen     funArgs  timeWhole' (stdError timeWholeSeries)
                                            timeSep'  (stdError timeSepSeries)
      --
      return (funs, funLen, funArgs, timeWhole', timeWholeSeries, timeSep', timeSepSeries)
  putStrLn "----------------------------------"
  putStrLn $ unlines (map show rawData)

timeProcess :: String -> [String] -> IO (Double, [Double])
timeProcess c args =
   replicateM 5 timing >>= (\times -> return $ (sum times / 5.0, times))
  where
   timing = do
     start <- getTime Monotonic
     _     <- system ("/usr/bin/time " ++ c ++ " " ++ unwords args ++ " >> log 2>&1")
     end   <- getTime Monotonic
     return $ fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10^(9 :: Integer)::Double)
