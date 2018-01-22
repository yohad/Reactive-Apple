{-# LANGUAGE FlexibleContexts #-}
module Apple where

newtype Axiom s t i = Axiom { axiom :: Image s t i -> Model s t i -> Image s t i }
data Model s t i = Model { is ::  [Image s t i], axs :: [Axiom s t i] }
newtype Image s t i = Image { projection :: s -> t -> i }

runModel :: (Ord t) => Model s t i -> t -> [Image s t i]
runModel m@(Model is axs) t  = fmap (\i -> foldl (\x f -> axiom f x m) i axs) is

transform :: Image s t i -> (i -> i) -> Image s t i
transform (Image p) f = Image (\s t -> f $ p s t)
