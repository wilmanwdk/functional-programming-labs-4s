module Test where
    
solveQuadratic :: Float -> Float -> Float -> [Float]
solveQuadratic a b c
    | a == 0 = [(-c)/b]
    | b*b-4*a*c > 0 = [(-b+sqrt(b*b-4*a*c))/(2*a), (-b-sqrt(b*b-4*a*c))/(2*a)]
    | b*b-4*a*c == 0 = [(-b+sqrt(b*b-4*a*c))/(2*a)]
    | otherwise = []

main = do
    putStrLn "ax^2+bx+c"
    putStr "a = "
    inputA <- getLine
    putStr "b = "
    inputB <- getLine
    putStr "c = "
    inputC <- getLine
    let a = (read inputA :: Float)
    let b = (read inputB :: Float)
    let c = (read inputC :: Float)
    print (solveQuadratic a b c)