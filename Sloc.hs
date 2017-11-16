module Main where

import System.Process (readProcess)
import System.Directory (listDirectory, removeFile)
import Big.Big hiding (main)
import Control.Monad (forM_)
import Data.List (isSuffixOf)

main :: IO ()
main = do
  -- Header
  putStrLn "#funs   #funLen   #args   single   multi"
  forM_ [2,3,4] $ \funArgs ->
   forM_ [5,10,15] $ \funs ->
    forM_ [5,10..20] $ \funLen -> do
      putStrLn $ show funs ++ "  " ++ show funLen ++ "  " ++ show funArgs
      -- 1. Synthesise the whole program...
      bigSynthesise "whole" "big" funs funLen funArgs
      sloccount <- readProcess "sloccount" ["big.f90"] []
      putStrLn sloccount
      removeFile "big.f90"

      -- 2. Synthesis separate programs...
      bigSynthesise "separate" "big" funs funLen funArgs
      items <- listDirectory "."
      let files = filter ("f90" `isSuffixOf`) items
      sloccount <- readProcess "sloccount" files []
      putStrLn sloccount
      forM_ files removeFile