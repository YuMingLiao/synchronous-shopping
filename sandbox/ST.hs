import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as B
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.IO as IO
import qualified Data.HashTable.ST.Cuckoo as C


main = do
  print $ foo

foo = runST $ do
    (ht :: B.HashTable s Int Int) <- H.new
    H.insert ht (1 :: Int) (1 :: Int)
    H.toList ht

-- seems I can't actually return ht cause s will be leaked.

count :: [String] -> [(String, Int)]
count ws = runST $ do
  (ht :: C.HashTable s String Int) <- H.new
  mapM_ (\w -> H.mutate ht w $ \v -> case v of
            Nothing -> (Just 1, ())
            Just x -> (Just (x+1), ())) ws
  H.toList ht


-- so give a tyep signature for ht can solve this issue.

-- 1. assign HashTable type at value after run action new.
-- 2. HashTable can't escape from ST monad, so use toList to turn it into a pure value.
