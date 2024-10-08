{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Prelude 
import Data.Foldable (foldl')
import qualified Data.List as L
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M hiding (insertLookupWithKey)
import qualified Data.Map.Strict as M (insertLookupWithKey)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Traversable as HM
import Data.Tuple (swap)
import Data.Text.Lazy (unpack)
import Data.Maybe (fromMaybe)
import Control.Exception (assert)
import Data.Ord (comparing)
import qualified Data.HashTable.ST.Basic as B
import qualified Data.HashTable.Class as H 
import Control.Monad.ST
import Control.Monad (foldM, when)
import Data.Functor (fmap)
import Data.Array.ST as A
import Data.Array.MArray as A
import Data.Array.Unboxed as A
import Data.Bits
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Hashable
{-
instance Hashable v => Hashable (S.Set v) where                                                                                                             
    hashWithSalt salt x = S.foldl' hashWithSalt (hashWithSalt salt (S.size x)) x
-}
data Heap a = Empty | Heap a [(Heap a)]
    deriving Show

singleton :: Ord a => a -> Heap a
singleton a = Heap a []

findMin :: Heap a -> a
findMin (Heap h _) = h

minView :: Ord a => Heap a -> Maybe (a, Heap a)
minView (Heap x hs) = Just (x, mergePairs hs)
minView Empty = Nothing

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h1@(Heap x hs1) h2@(Heap y hs2)
    | x < y     = Heap x (h2:hs1)
    | otherwise = Heap y (h1:hs2)

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs []           = Empty
mergePairs [h]          = h
mergePairs (h1:h2:hs)   = merge (merge h1 h2) (mergePairs hs)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Heap x [])

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Heap x hs) = mergePairs hs

showBits i = "0b" ++ (replicate (3 - length bits) ' ') ++ bits
  where
    bits = showIntAtBase 2 intToDigit i ""




main :: IO ()
main = do
  interact $ processInput
  where
    processInput input = show . round $ findShortestTwoPaths testData 
      where
        inputLines = lines input
        (firstLine, rest) = L.splitAt 1 inputLines
        firstLine' = L.map read . words $ head firstLine
        n = firstLine' !! 0
        k = firstLine' !! 2
        (cityLines, edgeLines) = L.splitAt n rest
        undirectedEdgeCostList = L.map (\(u:v:c:[]) -> ((u,v),Just (fromIntegral c))) . L.map (L.map read . words) $ edgeLines
        edgeCostAL = undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList
        testData = Test {
          numNodes = n,
          numFishTypes = k,
          fishTypeMap = debugId "fishTypeMap" $ A.array (1,n) . zipWith (,) [1..n] . L.map (S.fromList . L.drop 1 . L.map read) . L.map words $ cityLines,
          edgeCostMap = debugId "edgeCostMap" $ (A.listArray ((1,1),(n,n)) $ take (n*n) $ repeat Nothing) // edgeCostAL,
          adjacencyMap = debugId "adjacencyMap" $ (A.listArray (1,n) $ take n (repeat [])) // groupOn edgeCostAL 
        } 

groupOn l = go (debugId "sortOn" $ L.sortOn (fst.fst) l) []
  where
    go [] acc = acc
    go (((a,b),Just c):xs) [] = go xs [(a,[(b,c)])] 
    go (((a,b),Nothing):xs) acc = go xs acc 
    go (((a,b),Just c):xs) acc@(((a',elems):ys)) | a == a' = debugId "group" $ go xs ((a', (b,c):elems) :ys)
                                                 | a /= a' = debugId "not group" $ go xs ((a,[(b,c)]):acc)

debugFlag = False 
debug s a b | debugFlag = Debug.Trace.trace (s ++ ": " ++ show a) b 
            | otherwise = b   
debugId s a | debugFlag = Debug.Trace.trace (s ++ ": " ++ show a) a 
            | otherwise = a   
debugView s f a | debugFlag = Debug.Trace.trace (s ++ ": " ++ show (f a)) a 
                | otherwise = a

debugWhen pred | pred && debugFlag == True = debug
               | otherwise = \_ _ b -> b 

debugWhenId pred s a | pred a && debugFlag == True = debugId s a
                     | otherwise = a



type Time = Float 

{-
instance {-# Overlapping #-} Show Float where
  show t | t == 1/0 = "Infinity"
         | otherwise =  show $ round t
-}

data SourceOpt = Opt {
  timeUsed :: Time,
  source :: Vertex,
  bought :: Combination
} deriving Eq 

instance Ord SourceOpt where
  compare (Opt t1 v1 _) (Opt t2 v2 _) | compare t1 t2 == EQ = compare v1 v2
                                  | otherwise = compare t1 t2
 
type FishType = Int
type Vertex = Int

data Test = Test {
  numNodes :: Int,
  numFishTypes :: FishType,
  fishTypeMap :: A.Array Vertex (Set FishType),
  edgeCostMap :: A.Array (Vertex, Vertex) (Maybe Time),
  adjacencyMap :: A.Array Vertex [(Vertex, Time)]
 }


findShortestTwoPaths testData@(Test {..}) = go sortedList 
  where
    finalState = findFinalState testData
    sortedList = L.sortOn snd $ HM.toList finalState
    completions = L.filter f [(x,y) |  x <- sortedList, y <- sortedList]
    universalSet :: Integer
    universalSet = 2^numFishTypes - 1
    f ((s1,_),(s2,_)) = (s1 .|. s2) == universalSet
    go sortedList = (\((_,t1),(_,t2))-> max t1 t2) $ head $ L.sortOn sortFunc completions
    sortFunc ((_,t1),(_,t2)) = max t1 t2 

findFinalState :: Test -> HashMap Combination Time
findFinalState testData@(Test {..}) =  
  let r = runSTUArray (dijkstra testData 1 :: ST s (NodeState s))
      finalStateHM = HM.fromList $ loopF 0 [] r
  in finalStateHM 
  where
    universalSet = 2^numFishTypes - 1
    loopF i acc arr | i > universalSet = acc
                    | otherwise =
                      let e = arr A.! (numNodes,i)
                      in loopF (i+1) ((i,e) : acc) arr



type Combination = Integer
type NodeState s = STUArray s (Vertex,Combination) Time

dijkstra (Test {..}) start = do 
  let universalSet :: Integer
      universalSet = 2^numFishTypes - 1 
      startFishTypes :: Integer
      startFishTypes = foldl' f zeroBits $ fishTypeMap A.! start
      f :: Integer -> Int -> Integer
      f b a = b .|. bit (a-1)
      s = Opt 0 start startFishTypes
      initialQueue = singleton s
  state <- A.newArray ((1,0),(numNodes,universalSet)) (1/0)
  A.writeArray state (start,startFishTypes) 0 
  go state initialQueue 
  where
    go state (minView -> Nothing) = pure state
    go state (minView -> Just ((Opt t u mask), pq)) = do
      pq' <- foldM step pq adjs
      go state pq'
      where
        adjs = adjacencyMap A.! u
        step queue (v,cost) = do 
          let !mask' = mask .|. vFishTypes
              !tmp = t + cost
          orig <- A.readArray state (v,mask')
          case (tmp < orig) of
               True -> do
                 A.writeArray state (v,mask') tmp
                 pure accQueue
                 where 
                   sourceOpt = Opt tmp v mask'
                   accQueue = insert sourceOpt queue
               False -> do
                 pure queue
          where
            vFishTypes = foldl' f zeroBits $ fishTypeMap A.! v
            f :: Integer -> Int -> Integer
            f b a = b .|. bit (a-1)
            

