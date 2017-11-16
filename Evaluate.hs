{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Process (system)
import System.Directory (listDirectory, removeFile)
import Big.Big hiding (main)
import Control.Monad (forM_, forM, replicateM)
import Data.List (isPrefixOf, isSuffixOf, unwords)
import System.Clock
import Text.Printf

main :: IO ()
main = do
  -- Header
  putStrLn "#funs   #funLen   #args   single   multi"
  forM_ [2,4..8] $ \funArgs ->
   forM_ [10,20..40] $ \funs ->
    forM_ [10,20..40] $ \funLen -> do

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

      -- Calculate the total and mean time to compile the mod files
      let modCompileTime = sum modTimes
      --let meanModCompileTime = modTimes / funs
      -- ... infer the units for the whole file
      timeSep <- timeProcess "camfort" ["units-infer", "big.f90", "-I ."]
      -- cleanup
      removeFile "big.f90"
      forM_ (filter ("mod_" `isPrefixOf`) items) removeFile
      items <- listDirectory "."
      forM_ (filter ("fsmod" `isSuffixOf`) items) removeFile

      -- Report
      printf "%2d      %2d        %2d      %0.3f   %0.3f\n"
              funs     funLen     funArgs  timeWhole (timeSep + modCompileTime)

timeProcess :: String -> [String] -> IO Float
timeProcess c args =
   replicateM 3 timing >>= (\times -> return $ sum times / 3.0)
  where
   timing = do
     start <- getTime ThreadCPUTime
     _     <- system (c ++ " " ++ unwords args ++ " 1>&2")
     end   <- getTime ThreadCPUTime
     return $ fromIntegral (toNanoSecs end - toNanoSecs start) / (10^(9 :: Integer))
