{-# LANGUAGE FlexibleContexts #-}
module Apple where

import Data.Vector.Storable( (!)
                  , Vector
                  , fromList
                  , toList )
import Numeric.AD
import Numeric.Optimization.Algorithms.HagerZhang05

newtype Image s i = Image { project :: s -> i }

collision b1 b2 =
  let
    params = defaultParameters
    f :: (Num a) => v a -> a
    f = project b1 ^2 + project b2 ^2
    func = VFunction f
    g = VGradient $ grad f
  in
    optimize params 0 (fromList [0,0]) func
