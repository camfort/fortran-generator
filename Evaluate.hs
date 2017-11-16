{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Process (system)
import System.Directory (listDirectory, removeFile)
import Big.Big hiding (main)
import Control.Monad (forM_, forM, replicateM)
import Data.List (isPrefixOf, unwords)
import System.Clock
import Text.Printf

main :: IO ()
main = do
  -- Header
  putStrLn "#funs   #funLen   #args   single   multi"
  forM_ [2,4..8] $ \funArgs ->
   forM_ [10,20..40] $ \funs ->
    forM_ [10,20..40] $ \funLen -> do
      printf "%2d      %2d        %2d      " funs funLen funArgs

      -- 1. Synthesise the whole program...
      bigSynthesise "whole" "big" funs funLen funArgs
      -- ... infer its units, recording the time
      timeWhole <- timeProcess "camfort" ["units-infer", "big.f90"]
      -- cleanup
      removeFile "big.f90"
      printf "%0.3f   " timeWhole

      -- 2. Synthesis separate programs...
      bigSynthesise "separate" "big" funs funLen funArgs
      -- ... units-compile the module files
      items <- listDirectory "."
      modTimes <-
        forM (filter ("mod_" `isPrefixOf`) items) $ \modFile ->
          timeProcess "camfort" ["units-compile", modFile, modFile]

      -- Calculate the total and mean time to compile the mod files
      let modCompileTime = sum modTimes
      --let meanModCompileTime = modTimes / funs
      -- ... infer the units for the whole file
      timeSep <- timeProcess "camfort" ["units-infer", "big.f90", "-I ."]
      -- cleanup
      removeFile "big.f90"
      forM_ (filter ("mod_" `isPrefixOf`) items) $ \modFile ->
          removeFile modFile

      -- Report
      printf "%0.3f" (timeSep + modCompileTime)

timeProcess :: String -> [String] -> IO Float
timeProcess c args =
   replicateM 3 timing >>= (\times -> return $ sum times / 3.0)
  where
   timing = do
     start <- getTime ThreadCPUTime
     _     <- system (c ++ " " ++ unwords args ++ " 1>/dev/null 2>/dev/null")
     end   <- getTime ThreadCPUTime
     return $ fromIntegral (toNanoSecs end - toNanoSecs start) / (10^(9 :: Integer))
