{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Process (system)
import System.Directory (listDirectory, removeFile)
import Big.Big hiding (main)
import Control.Monad (forM_, forM, replicateM)
import Data.List (isPrefixOf, isSuffixOf, unwords)
import System.Clock
import System.IO
import Text.Printf
import StdError


-- some parameters
profiling = True

repeatTimes = 5

-- (funArgsRange, funsRange, funLenRange) = ([2], [5], [5])
(funArgsRange, funsRange, funLenRange) = ([2,4], [5,10 .. 20], [5, 10 .. 20])


--------------------------------------------------

cleanModFiles = do
  items <- listDirectory "."
  forM_ (filter ("fsmod" `isSuffixOf`) items) removeFile

profileOpts filename | profiling = "+RTS -p -po" ++ filename
                     | otherwise = ""

main :: IO ()
main = do
  -- Header
  putStrLn "#funs   #funLen   #args   single    (err)      mod      (err)      multi    (err)      mod+multi    single/(mod+multi)"
  hFlush stdout
  rawData <-
   forM funArgsRange $ \funArgs ->
    forM funsRange $ \funs ->
     forM funLenRange $ \funLen -> do

      -- 1. Synthesise the whole program...
      bigSynthesise "whole" "big" funs funLen funArgs
      -- ... infer its units, recording the time
      let wholeProfFile = printf "whole-%d-%d-%d" funArgs funs funLen
      timeWhole <- timeProcess "camfort" ["units-infer", "big.f90", profileOpts wholeProfFile]
      -- cleanup
      removeFile "big.f90"

      -- 2. Synthesis separate programs...
      bigSynthesise "separate" "big" funs funLen funArgs
      -- ... units-compile the module files
      -- items <- listDirectory "."
      -- modTimes <-
        -- forM (filter ("mod_" `isPrefixOf`) items) $ \modFile ->
          -- timeProcess "camfort" ["units-compile", modFile]
      -- Calculate the total time to compile the mod files
      let modProfFile = printf "mod-%d-%d-%d" funArgs funs funLen
      modCompileTime <- timeProcess' cleanModFiles "camfort" ["units-compile", ".", profileOpts modProfFile]

      -- ... infer the units for the whole file
      let sepProfFile = printf "sep-%d-%d-%d" funArgs funs funLen
      timeSep <- timeProcess "camfort" ["units-infer", "big.f90", "-I .", profileOpts sepProfFile]
      -- cleanup
      removeFile "big.f90"
      items <- listDirectory "."
      forM_ (filter ("mod_" `isPrefixOf`) items) removeFile
      cleanModFiles

      let (timeWhole', timeWholeSeries) = timeWhole
      let (timeMod', timeModSeries) = modCompileTime
      let (timeSep', timeSepSeries) = timeSep
      -- Report
      printf "%2d      %2d        %2d      %6.2f    (%6.2f)   %6.2f   (%6.2f)   %6.2f   (%6.2f)   %6.2f         %6.2f\n"
              funs     funLen     funArgs  timeWhole' (stdError timeWholeSeries)
                                            timeMod'  (stdError timeModSeries)
                                            timeSep'  (stdError timeSepSeries)
                                            (timeMod' + timeSep')
                                            (timeWhole' / (timeMod' + timeSep'))
      hFlush stdout
      --
      return (funs, funLen, funArgs, timeWhole', timeWholeSeries, timeMod', timeModSeries, timeSep', timeSepSeries, timeMod' + timeSep')
  putStrLn "----------------------------------"
  putStrLn $ unlines (map show rawData)

timeProcess :: String -> [String] -> IO (Double, [Double])
timeProcess = timeProcess' (return ())

timeProcess' :: IO () -> String -> [String] -> IO (Double, [Double])
timeProcess' preAction c args =
   replicateM repeatTimes timing >>= (\times -> return $ (sum times / fromIntegral repeatTimes, times))
  where
   timing = do
     preAction
     start <- getTime Monotonic
     let cmd = "/usr/bin/time " ++ c ++ " " ++ unwords args ++ " >> log 2>&1"
     _     <- system $ "echo " ++ cmd
     _     <- system cmd
     end   <- getTime Monotonic
     return $ fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10^(9 :: Integer)::Double)
