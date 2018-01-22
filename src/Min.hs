module Min where

gradient :: ([Double] -> Double) -> [Double] -> Double -> [Double]
gradient _ [] _ = []
gradient f x0 eps =
  let
    expand :: [Double] -> [Double] -> [[Double]]
    expand _  [] = []
    expand as bs = (as ++ [head bs + eps] ++ tail bs) : expand (as ++ [head bs]) (tail bs)
  in
    map (\x1 -> (f x1 - f x0) / eps) (expand [] x0)
