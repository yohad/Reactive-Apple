{-# LANGUAGE DataKinds #-}
module Apple where

import Data.Vector.Storable( (!)
                  , Vector
                  , fromList
                  , toList )
import Numeric.GSL.Minimization

data Body = Body { pos :: (Vector Double) -> Double }

circle :: Double -> Double -> Body
circle x y = Body (\v -> (v!0-x)^2 + (v!1-y)^2 - 1)

--collision :: Body -> Body -> Bool
collision b1 b2 =
    let
        p1 = pos b1
        f [b1, b2] = b1^2 + b2^2
    in
        minimize NMSimplex2 1E-2 50 [1,1] f
