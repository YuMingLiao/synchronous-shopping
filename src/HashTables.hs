{-# LANGUAGE PackageImports #-}
module HashTables where
import qualified "hashtables" Data.HashTable.IO as H

type HashTable k v = H.BasicHashTable k v

main :: IO (HashTable Int Int)
main = do
    ht <- H.new
    H.insert ht 1 1
    return ht
