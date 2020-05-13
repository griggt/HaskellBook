module Review where

-- exercise 1
stops = "pbtdkg"
vowels = "aeiou"

makeWords :: [String]
makeWords = [[s,v,s'] | s <- stops, v <- vowels, s' <- stops ]

makePwords :: [String]
makePwords = [[s,v,s'] | s <- stops, v <- vowels, s' <- stops, s == 'p']

nouns = ["desk", "chair", "lamp", "wallet"]
verbs = ["drinks", "runs", "writes", "draws"]
makeSentences :: [String]
makeSentences = [concat [n," ",v," ",n'] | n <- nouns, v <- verbs, n' <- nouns]

--exercise 2
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))
-- computes average number of characters per word in the sentence 'x'

-- exercise 3
avgCharsWord x =
    (/) (fromIntegral (sum (map length (words x))))
        (fromIntegral (length (words x)))
