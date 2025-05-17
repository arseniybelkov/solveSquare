module Solver (solveSquare) where


data Roots f = NoRoots | SingleRoot f | TwoRoots f f deriving (Show)

class (Floating f, Ord f, Fractional f) => FloatingPoint f where

instance FloatingPoint Float where
instance FloatingPoint Double where


kEpsilon :: FloatingPoint f => f
kEpsilon = 1e-10


isZero :: FloatingPoint f => f -> Bool
isZero x = abs x < kEpsilon


solveSquare :: FloatingPoint f => f -> f -> f -> Roots f
solveSquare a b c 
    | isZero a = maybe NoRoots SingleRoot (solveLinear b c)
    | otherwise = findQuadraticRoots a b c


findQuadraticRoots :: FloatingPoint f => f -> f -> f -> Roots f
findQuadraticRoots a b c = case calculateDisc a b c of 
    Just sqrt_d -> TwoRoots ((-b - sqrt_d ) / (2 * a)) ((-b + sqrt_d ) / (2 * a))
    Nothing -> NoRoots 


calculateDisc :: FloatingPoint f => f -> f -> f -> Maybe f
calculateDisc a b c
    | disc >= 0.0 = Just (sqrt disc)
    | otherwise = Nothing
    where disc = b * b - 4 * a * c


solveLinear :: FloatingPoint f => f -> f -> Maybe f
-- ax + b = 0
solveLinear a b
    | isZero a = Nothing
    | otherwise = Just (-b / a)