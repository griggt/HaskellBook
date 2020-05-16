module Maybe where

-- exercise #1
-- in Scala, this is Option.isDefined or Option.nonEmpty
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

-- in Scala, this is Option.isEmpty
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

-- exercise #2
-- in Scala, this is Option.fold()
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing  = z
mayybee _ f (Just x) = f x

-- exercise #3
-- in Scala, this is Option.getOrElse
fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing  = z
fromMaybe _ (Just x) = x

-- exercise #4
-- in Scala, this is List.headOption (from IterableOps)
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

-- in Scala, this is Option.toList
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- exercise #5
catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes (Just x:xs)   = x : catMaybes xs

-- exercise #6
--   this is a specialization of Traversable.sequence (we will learn later?!)
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []          = Nothing
flipMaybe (Nothing:_) = Nothing
flipMaybe (Just x:[]) = Just [x] 
flipMaybe (Just x:xs) = 
  case flipMaybe xs of 
     Nothing -> Nothing
     Just ys -> Just (x:ys)
