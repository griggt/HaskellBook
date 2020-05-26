module FixerUpper where

-- exercise #1
ex1 = const <$> Just "Hello" <*> pure "World"

-- exercise #2
ex2 = (,,,) <$> Just 90 
            <*> Just 10 
            <*> Just "Tierness" 
            <*> pure [1, 2, 3]
