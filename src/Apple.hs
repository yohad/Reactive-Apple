{-# LANGUAGE DataKinds #-}
module Apple where

import Data.Vector.Storable( (!)
                  , Vector
                  , fromList
                  , toList )
import Numeric.AD
import Numeric.Optimization.Algorithms.HagerZhang05

data Body = Body { pos :: (Vector Double) -> Double }

circle :: Double -> Double -> Body
circle x y = Body (\v -> (v!0-x)^2 + (v!1-y)^2 - 1)

