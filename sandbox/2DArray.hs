import GHC.Arr (Array, array)

toArray :: [[a]] -> Array (Int, Int) a
toArray vss
  = array ((1,1), (w,h))
    [ ((x,y), v)
    | (y, vs) <- zip [1..] vss
    , (x, v) <- zip [1..] vs
    ]
  where
    w = case vss of
      [] -> 0
      vs:_ -> length vs
    h = length vss

-- immutable, so lookup O(1), modification O(width*hieght)
