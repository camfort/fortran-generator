module Main where

import System.Process (system)
import Control.Monad (replicateM, forM)
import System.Clock
import StdError
import System.Environment

main :: IO ()
main = do
  -- Header
  args <- getArgs
  if length args == 0
   then putStrLn "Please supply at least one file name"
   else do
     dependencies <- forM (tail args) (\f -> timeProcess "camfort" ["units-compile", f])
     fileTop <- timeProcess "camfort" ["units-infer", head args, "-I ."]
     let times = sum dependencies + fileTop
     putStrLn (show (fst times) ++ " +- " ++ show (stdError (snd times)))

timeProcess :: String -> [String] -> IO (Double, [Double])
timeProcess c args =
   replicateM 5 timing >>= (\times -> return $ (sum times / 5.0, times))
  where
   timing = do
     start <- getTime Monotonic
     _     <- system (c ++ " " ++ unwords args ++ " >> log 2>&1")
     end   <- getTime Monotonic
     return $ fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10^(9 :: Integer)::Double)
