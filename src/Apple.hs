module Apple where

import qualified Data.Vector as V
import Numeric.LinearAlgebra

data Reversible a b = Reversible { forward :: a -> b
                                 , backward :: b -> a}

revert :: Reversible a b -> Reversible b a
revert r = Reversible (backward r) (forward r)

data Body a b = Body {unwrapList :: [(a -> b, a, a)]}
instance Show a => Show (Body a b) where
    show = show . map (\(f,mn, mx) -> (mn, mx)) . unwrapList

class Boundry a where
    (>|) :: a -> a -> Bool
    (|<) :: a -> a -> Bool
    (|<) = flip (>|)

instance (Num a, Ord a) => Boundry (V.Vector a) where
    x >| y = V.all (>0) $ fmap (\(a,b) -> a-b) (V.zip x y)

transform :: (Boundry a, Boundry c) => Reversible a c -> Body a b -> Body c b
transform g = Body . Prelude.map (\(f, mn ,mx) -> (f . backward g, forward g mn, forward g mx)) . unwrapList

circle :: Body (V.Vector Double) Double
circle = Body [(\v -> (V.sum . fmap (**2)) v - 1, V.fromList [-1, -1], V.fromList [1, 1])]

moveX :: Num a => V.Vector a -> V.Vector a
moveX v =  V.fromList [V.head v + 10] V.++ (V.tail v)

movemX :: Num a => V.Vector a -> V.Vector a
movemX v = V.fromList [V.head v - 10] V.++ (V.tail v)

move :: Num a => Reversible (V.Vector a) (V.Vector a)
move = Reversible moveX movemX
