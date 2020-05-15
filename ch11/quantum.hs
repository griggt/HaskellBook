module Quantum where

data Quantum = Yes | No | Both 
    deriving (Eq, Show)

convert :: Quantum -> Bool
convert  = undefined

convert1 Yes  = True
convert1 No   = True
convert1 Both = True

convert2 Yes  = False
convert2 No   = True
convert2 Both = True

convert3 Yes  = True
convert3 No   = False
convert3 Both = True

convert4 Yes  = True
convert4 No   = True
convert4 Both = False

convert5 Yes  = False
convert5 No   = False
convert5 Both = True

convert6 Yes  = False
convert6 No   = True
convert6 Both = False

convert7 Yes  = True
convert7 No   = False
convert7 Both = False

convert8 Yes  = False
convert8 No   = False
convert8 Both = False