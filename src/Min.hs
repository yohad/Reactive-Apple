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

minimize :: ([Double] -> Double) -> [Double] -> Double -> Double -> [[Double]]
minimize f x0 eps delta | f x0 **2 < eps = [x0]
                        | otherwise = x0 : minimize f (vectorSub x0 $ vectorSMul delta $ gradient f x0 eps) eps delta

vectorSub :: [Double] -> [Double] -> [Double]
vectorSub [] [] = []
vectorSub (x:xs) (y:ys) = (x - y) : vectorSub xs ys

vectorSMul :: Double -> [Double] -> [Double]
vectorSMul s = map (s*)

test = minimize (\(x:y:_) -> x**2 + y**2) [1,1] 1e-8 0.5
