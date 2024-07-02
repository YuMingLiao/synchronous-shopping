module HashTableOfHashTable where
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Control.Monad.ST

type HashTable s k v = C.HashTable s k v

foo :: ST s (HashTable s Int (HashTable s k v))
foo = do
    ht <- H.new
    ht' <- H.new
    H.insert ht' 1 1
    H.insert ht 1 ht'
    (Just ht'') <- H.lookup (1 :: Int) ht
    return ht''


-- seems HashTable of HashTable is troublesome.
