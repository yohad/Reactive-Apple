{-# LANGUAGE DataKinds #-}
module Apple where

import Data.Vector.Storable( (!)
                  , Vector
                  , fromList
                  , toList )
import Numeric.AD
import Numeric.Optimization.Algorithms.HagerZhang05



