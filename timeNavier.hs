module Main where

import System.Process (system)
import Control.Monad (replicateM)
import System.Clock

main :: IO ()
main = do
  -- Header
  file1 <- timeProcess "camfort" ["units-compile", "helpers_mod.f90"]
  file2 <- timeProcess "camfort" ["units-compile", "init_mod.f90"]
  file3 <- timeProcess "camfort" ["units-compile", "boundary_mod.f90"]
  file4 <- timeProcess "camfort" ["units-compile", "output_mod.f90"]
  file5 <- timeProcess "camfort" ["units-compile", "simulation_mod.f90"]
  fileTop <- timeProcess "camfort" ["units-infer", "navier.f90", "-I ."]
  putStrLn (show $ file1 + file2 + file3 + file4 + file5 + fileTop)

timeProcess :: String -> [String] -> IO Double
timeProcess c args =
   replicateM 3 timing >>= (\times -> return $ sum times / 3.0)
  where
   timing = do
     start <- getTime Monotonic
     _     <- system ("/usr/bin/time " ++ c ++ " " ++ unwords args ++ " >> log 2>&1")
     end   <- getTime Monotonic
     return $ fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10^(9 :: Integer)::Double)
