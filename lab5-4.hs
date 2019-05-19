module Test where

-- actions = ["ADD", "EDIT", "PRINT"]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

removeAt :: Int -> [(String, String, Integer)] -> [(String, String, Integer)]
removeAt index list
    | index < length list = (fst (splitAt (index) list)) ++ (tail (snd (splitAt (index) list)))
    | otherwise = list

findByArticle' :: String -> [(String, String, Integer)] -> Int -> Int
findByArticle' article db num
    | null db = -1
    | fst3 (head db) == article = num
    | otherwise = findByArticle' article (tail db) (num+1)

findByArticle article db = findByArticle' article db 0

findByName' :: String -> [(String, String, Integer)] -> Int -> Int
findByName' name db num
    | null db = -1
    | snd3 (head db) == name = num
    | otherwise = findByName' name (tail db) (num+1)

findByName name db = findByName' name db 0

addItem :: String -> String -> Integer -> [(String, String, Integer)] -> [(String, String, Integer)]
addItem article name amount db
    | article == "" || name == "" = db
    | otherwise = db ++ [(article, name, amount)]

editItemByArticle :: String -> Integer -> [(String, String, Integer)] -> [(String, String, Integer)]
editItemByArticle article amount db
    | index == -1 = db
    | otherwise = removeAt index db ++ [(fst3 (db !! index), snd3 (db !! index), amount)]
    where index = findByArticle article db

editItemByName :: String -> Integer -> [(String, String, Integer)] -> [(String, String, Integer)]
editItemByName name amount db
    | index == -1 = db
    | otherwise = removeAt index db ++ [(fst3 (db !! index), snd3 (db !! index), amount)]
    where index = findByName name db

printDB' db index = do
    if length db == index then putStrLn ""
    else do
        putStr "Article: "
        putStr (fst3 (db !! index))
        putStr " Name: "
        putStr (snd3 (db !! index))
        putStr " Amount: "
        print (thd3 (db !! index))
        printDB' db (index + 1)
        
performAction db = do
    putStr "Action: "
    action <- getLine
    if action == "add" then do
        putStr "Article: "
        article <- getLine
        putStr "Name: "
        name <- getLine
        putStr "Amount: "
        inputAm <- getLine
        let amount = (read inputAm :: Integer)
        performAction (addItem article name amount db)
    else if action == "edit" then do
        putStr "Edit by (a)rticle or by (n)ame: "
        key <- getLine
        if key == "a" || key == "A" then do
            putStr "Article: "
            article <- getLine
            putStr "Amount: "
            inputAm <- getLine
            let amount = (read inputAm :: Integer)
            performAction (editItemByArticle article amount db)
        else if key == "n" || key == "N" then do
            putStr "Name: "
            name <- getLine
            putStr "Amount: "
            inputAm <- getLine
            let amount = (read inputAm :: Integer)
            performAction (editItemByName name amount db)
        else do
            putStrLn "Invalid option"
            performAction db
    else if action == "print" then do
        printDB' db 0
        performAction db
    else if action == "exit" then
        putStrLn "ok"
    else do
        putStrLn "Invalid action"
        performAction db

main = do
    performAction []