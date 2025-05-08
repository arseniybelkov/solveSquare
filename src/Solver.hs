module Solver (solveSquare) where


data Roots = None | One Double | Two Double Double deriving (Show)


solveSquare :: Double -> Double -> Double -> Roots
solveSquare a b c 
    | abs a < 1e-10 = intoRoots (solveLinear b c)
    | otherwise = findQuadraticRoots a b c


findQuadraticRoots :: Double -> Double -> Double -> Roots
findQuadraticRoots a b c = case calculateDisc a b c of 
    Just sqrt_d -> Two ((-b - sqrt_d ) / (2 * a)) ((-b + sqrt_d ) / (2 * a))
    Nothing -> None


calculateDisc :: Double -> Double -> Double -> Maybe Double
calculateDisc a b c
    | disc >= 0.0 = Just (sqrt disc)
    | otherwise = Nothing
    where disc = b * b - 4 * a * c
        

intoRoots :: Maybe Double -> Roots
intoRoots (Just x) = One x
intoRoots Nothing = None


solveLinear :: Double -> Double -> Maybe Double
-- ax + b = 0
solveLinear a b
    | abs a < 1e-10 = Nothing
    | otherwise = Just (-b / a)