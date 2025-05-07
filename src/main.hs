data Roots = None | One Double | Two Double Double


solveSquare :: Double -> Double -> Double -> Roots
solveSquare a b c
    | abs a < eps 
    where eps = 1e-8

solveLinear :: Double -> Double -> Maybe Double
-- ax + b = 0
solveLinear a b
    | abs a < eps = Nothing
    | _ = Just -(b / a)
    where eps = 1e-8