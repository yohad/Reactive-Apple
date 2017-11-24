{-# LANGUAGE FlexibleContexts #-}
module Apple where

import Data.Vector.Storable( (!)
                  , Vector
                  , fromList
                  , toList )
import Numeric.GSL.Minimization

newtype Image s i = Image { project :: s -> i }

collision b1 b2 =
  let
    f p = project b1 p ^2 + project b2 p ^2
  in
    minimize NMSimplex2 1E-2 30 [1,1] f [0,0]

circle x0 y0 r = Image (\[x,y] -> (x-x0)^2 + (y-y0)^2 - r^2)
