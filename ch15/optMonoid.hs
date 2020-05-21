module OptionalMonoid where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    Nada   <> y      = y
    x      <> Nada   = x 
    Only x <> Only y = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
