{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Process (system)
import System.Directory (listDirectory, removeFile)
import Big.Big hiding (main)
import Control.Monad (forM_, forM)
import Data.List (isPrefixOf, unwords)
import System.Clock
import Text.Printf

main :: IO ()
main = do
  -- Header
  putStrLn "#funs   #funLen   #args   single   multi"
  forM_ [10,20..40] $ \funs ->
   forM_ [10,20..40] $ \funLen ->
    forM_ [5,10..20] $ \funArgs -> do
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
      reportLine funs funLen funArgs timeWhole (timeSep + modCompileTime)

timeProcess :: String -> [String] -> IO Float
timeProcess c args = do
  start <- getTime Realtime
  _     <- system (c ++ " " ++ unwords args ++ " 1>/dev/null 2>/dev/null")
  end   <- getTime Realtime
  return $ fromIntegral (nsec (end - start)) / (10^(8 :: Integer))

reportLine :: Int -> Int -> Int -> Float -> Float -> IO ()
reportLine = printf "%2d      %2d        %2d      %3.3f   %3.3f\n"
