import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime 
                (fromGregorian 1911 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime 
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 8
    ]


-- exercise 1
-- TODO:
--   can I use the builtin filter function rather than writing extract?

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = concat . map extract $ xs 
    where 
      extract (DbDate t) = [t]
      extract _          = []

-- exercise 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = concat . map extract $ xs
    where
      extract (DbNumber n) = [n]
      extract _            = []

-- exercise 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- exercise 4
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

-- exercise 5
avgDb :: [DatabaseItem] -> Double
avgDb xs = sum / count where
    sum = fromIntegral (sumDb xs)
    count = fromIntegral (length $ filterDbNumber xs)

avgDb' :: [DatabaseItem] -> Double
avgDb' xs = fromIntegral sum / count where
    (sum, count) = foldr (\a (b, c) -> (a + b, c + 1)) (0, 0) . filterDbNumber $ xs
