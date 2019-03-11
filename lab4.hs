module Test where
import Data.Char
import Data.List

-- 1
removeDuplicates :: [Char] -> [Char] -> [Char]
removeDuplicates str seen
	| str == "" && seen == "" = ""
	| seen == "" = removeDuplicates (tail str) ([head str])
	| str == "" = seen
	| head str == last seen = removeDuplicates (tail str) (seen)
	| otherwise = removeDuplicates (tail str) (seen ++ [head str])
	
rd str = removeDuplicates str ""

-- 2
stringBuilder :: [(Char, Int)] -> [Char] -> [Char]
stringBuilder rules done
	| null rules = done
	| otherwise = stringBuilder (tail rules) (done ++ replicate (snd (head rules)) (fst (head rules)))
	
sb rules = stringBuilder rules ""
	
-- 3
filterDigits :: [Char] -> [Char]
filterDigits list = filter isDigit list

-- 4
isPalindrome str
	| str == "" || length str == 1 = True
	| head str == last str = isPalindrome (tail (init str))
	| otherwise = False
	
countPalindromes list = length (filter isPalindrome (words list))

-- 5
cmpWords a b
	| map toUpper a > map toUpper b = GT
	| otherwise = LT

splitnsort text = reverse (sortBy cmpWords (words (text)))

-- 6
vowels = ['a', 'e', 'y', 'u', 'i', 'o']

capitalizeIfVowel :: Char -> Char
capitalizeIfVowel char
	| null (findIndices (==char) vowels) = char
	| otherwise = toUpper char 

capitalizeVowels :: [Char] -> [Char]
capitalizeVowels word = map capitalizeIfVowel word