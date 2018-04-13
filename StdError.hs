{-# LANGUAGE FlexibleInstances #-}

module StdError where

instance Num (Double, [Double]) where
  (x, ds) + (x', ds') = (x+x', ds++ds')
  fromInteger n = (fromInteger n, [])

stdDeviation :: [Double] -> Double
stdDeviation xs = sqrt (divisor * (sum . map (\x -> (x - mean)**2) $ xs))
  where
   divisor = 1 / ((cast n) - 1)
   n = length xs
   mean = sum xs / (cast n)

cast :: Int -> Double
cast = fromInteger . toInteger

stdError :: [Double] -> Double
stdError xs = (stdDeviation xs) / sqrt (cast n)
  where
    n = length xs