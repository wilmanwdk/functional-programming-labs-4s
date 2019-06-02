import Control.Monad.Par
import Data.Time

main = do
    start <- getCurrentTime
    putStrLn $ show h
    stop <- getCurrentTime
    print(diffUTCTime stop start)

h = runPar $ do
    fx <- spawnP (f 0 100000000)
    gx <- spawnP (g 0 100000000)
    a <- get fx
    b <- get gx
    return (a + b)

f::Double->Double->Double
f sum x = if x>0 then f(sum+x)(x-1) else sum
g::Double->Double->Double
g sum x = if x>0 then g(sum+x)(x-1) else sum