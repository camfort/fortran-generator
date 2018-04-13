module Main where

import System.Process (system)
import Control.Monad (replicateM)
import System.Clock
import StdError

main :: IO ()
main = do
  -- Header
  file1 <- timeProcess "camfort" ["units-infer", "navierWhole.f90"]
  putStrLn (show $ fst file1)
  putStrLn ("+-" ++ (show $ stdError (snd file1)))

timeProcess :: String -> [String] -> IO (Double, [Double])
timeProcess c args =
   replicateM 5 timing >>= (\times -> return $ (sum times / 3.0, times))
  where
   timing = do
     start <- getTime Monotonic
     _     <- system (c ++ " " ++ unwords args ++ " >> log 2>&1")
     end   <- getTime Monotonic
     return $ fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10^(9 :: Integer)::Double)
