import Data.Foldable (for_)
import GHC.IOArray (IOArray, newIOArray, writeIOArray)

toIOArray :: [[a]] -> IO (IOArray (Int, Int) a)
toIOArray vss = do
  let w = case vss of
            [] -> 0
            vs:_ -> length vs
  let h = length vss
  arr <- newIOArray ((1,1), (w,h)) undefined
  for_ (zip [1..] vss) $ \(y, vs) -> do
    for_ (zip [1..] vs) $ \(x, v) -> do
      writeIOArray arr (x,y) v
  pure arr
