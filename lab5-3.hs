module Test where
import Data.Char
    
removePunc :: String -> String
removePunc str = [c | c <- str, not (c `elem` ",.?!-:;\"\'")]

strToLower :: String -> String
strToLower str = [toLower c | c <- str]

wordIndexInList' :: [Char] -> [([Char], Int)] -> Int -> Int
wordIndexInList' word list num
    | null list = -1
    | fst (head list) == word = num
    | otherwise = wordIndexInList' word (tail list) (num+1)

wordIndexInList word list = wordIndexInList' word list 0

removeAt :: Int -> [([Char], Int)] -> [([Char], Int)]
removeAt index list
    | index < length list = (fst (splitAt (index) list)) ++ (tail (snd (splitAt (index) list)))
    | otherwise = list

updateWordInList :: [Char] -> [([Char], Int)] -> [([Char], Int)]
updateWordInList word list
    | index == -1 = list ++ [(word, 1)]
    | otherwise = removeAt index list ++ [(fst (list !! index), snd (list !! index) + 1)]
    where index = wordIndexInList word list

getTopWord' :: [([Char], Int)] -> ([Char], Int) -> ([Char], Int)
getTopWord' wordsDict current
    | null wordsDict = current
    | snd (head wordsDict) > snd current = getTopWord' (tail wordsDict) (head wordsDict)
    | otherwise = getTopWord' (tail wordsDict) (current)

getTopWord wordsDict = getTopWord' wordsDict ("", 0)

topFiveWords' :: [([Char], Int)] -> [([Char], Int)] -> [([Char], Int)]
topFiveWords' wordsDict result
    | null wordsDict || length result == 5 = result
    | otherwise = topFiveWords' (removeAt (wordIndexInList (fst topWord) wordsDict) wordsDict) (result ++ [topWord])
    where topWord = getTopWord wordsDict

topFiveWords wordsDict = topFiveWords' wordsDict []

countWords' :: [Char] -> [([Char], Int)] -> [([Char], Int)]
countWords' text wordsDict
    | null (words text) = wordsDict
    | otherwise = countWords' (unwords (tail (words text))) (updateWordInList (head (words text)) (wordsDict))

countWords text = countWords' text []

topFiveWordsInText :: [Char] -> [([Char], Int)]
topFiveWordsInText text = topFiveWords (countWords (removePunc (strToLower text)))

main = do
    text <- readFile("space-oddity.txt")
    print (topFiveWords (countWords (removePunc (strToLower text))))