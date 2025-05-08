module Solver (solveSquare) where


data Roots = NoRoots | SingleRoot Double | TwoRoots Double Double deriving (Show)


kEpsilon :: Double
kEpsilon = 1e-10


isZero :: Double -> Bool
isZero x = abs x < kEpsilon


solveSquare :: Double -> Double -> Double -> Roots
solveSquare a b c 
    | isZero a = maybe NoRoots SingleRoot (solveLinear b c)
    | otherwise = findQuadraticRoots a b c


findQuadraticRoots :: Double -> Double -> Double -> Roots
findQuadraticRoots a b c = case calculateDisc a b c of 
    Just sqrt_d -> TwoRoots ((-b - sqrt_d ) / (2 * a)) ((-b + sqrt_d ) / (2 * a))
    Nothing -> NoRoots 


calculateDisc :: Double -> Double -> Double -> Maybe Double
calculateDisc a b c
    | disc >= 0.0 = Just (sqrt disc)
    | otherwise = Nothing
    where disc = b * b - 4 * a * c


solveLinear :: Double -> Double -> Maybe Double
-- ax + b = 0
solveLinear a b
    | isZero a = Nothing
    | otherwise = Just (-b / a)