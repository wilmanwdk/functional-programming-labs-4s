module Test where

solveQuadratic :: Float -> Float -> Float -> [Float]
solveQuadratic a b c
    | a == 0 = [(-c)/b]
    | b*b-4*a*c > 0 = [(-b+sqrt(b*b-4*a*c))/(2*a), (-b-sqrt(b*b-4*a*c))/(2*a)]
    | b*b-4*a*c == 0 = [(-b+sqrt(b*b-4*a*c))/(2*a)]
    | otherwise = []

main = do
    solveQuadratic 1 1 (-6)