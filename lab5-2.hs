module Test where

fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
    putStr "operator: "
    op <- getLine
    putStr "a: "
    inputA <- getLine
    let a = (read inputA :: Float)
    if op == "+" then do
        putStr "b: "
        inputB <- getLine
        let b = (read inputB :: Float)
        print (a + b)
    else if op == "-" then do
        putStr "b: "
        inputB <- getLine
        let b = (read inputB :: Float)
        print (a - b)
    else if op == "*" then do
        putStr "b: "
        inputB <- getLine
        let b = (read inputB :: Float)
        print (a * b)
    else if op == "/" then do
        putStr "b: "
        inputB <- getLine
        let b = (read inputB :: Float)
        print (a / b)
    else if op == "^" then do
        putStr "b: "
        inputB <- getLine
        let b = (read inputB :: Float)
        print (a ** b)
    else if op == "sqrt" then
        print (sqrt a)
    else if op == "!" then
        print (fact a)
    else if op == "fib" then
        print (fib a)
    else
        putStr "Invalid operator"