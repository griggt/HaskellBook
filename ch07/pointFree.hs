module PointFree where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip'  = read . show

main = do
  print (roundTrip 4)
  print (id 4)

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 a = read (show a)

-- To invoke: print ((roundTrip2 4) :: Int)