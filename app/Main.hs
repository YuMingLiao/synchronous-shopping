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


showBits i = "0b" ++ (replicate (3 - length bits) ' ') ++ bits
  where
    bits = showIntAtBase 2 intToDigit i ""


data PriorityQueue k a = Nil | Branch k a (PriorityQueue k a) (PriorityQueue k a)

empty :: Ord k => PriorityQueue k a
empty = Nil

singleton :: Ord k => k -> a -> PriorityQueue k a
singleton k a = Branch k a Nil Nil

minKeyValue :: Ord k => PriorityQueue k a -> (k, a)
minKeyValue Nil              = error "empty queue"
minKeyValue (Branch k a _ _) = (k, a)


minView :: Ord k => PriorityQueue k a -> Maybe (a, PriorityQueue k a)
minView Nil              = Nothing
minView (Branch _ a l r) = Just (a, union l r)

union :: Ord k => PriorityQueue k a -> PriorityQueue k a -> PriorityQueue k a
union l Nil = l
union Nil r = r
union l@(Branch kl _ _ _) r@(Branch kr _ _ _)
    | kl <= kr  = link l r
    | otherwise = link r l

link (Branch k a Nil m) r = Branch k a r m
link (Branch k a ll lr) r = Branch k a lr (union ll r)

insert :: Ord k => k -> a -> PriorityQueue k a -> PriorityQueue k a
insert k a q = union (singleton k a) q

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
        testData = Test {
          numNodes = n,
          numFishTypes = k,
          fishTypeMap = HM.fromList . zipWith (,) [1..n] . L.map (S.fromList . L.drop 1 . L.map read) . L.map words $ cityLines,
          edgeCostMap = let 
            undirectedEdgeCostList = L.map (\(u:v:c:[]) -> ((u,v),fromIntegral c)) . L.map (L.map read . words) $ edgeLines
            in HM.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
          adjacencyMap = toAdjacencyMap (HM.keys (edgeCostMap testData))
        } 
 
-- print $ findShortestTwoPaths example 

debugFlag = True 
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


{-
newtype Time = Time {
  unTime :: Float
} deriving (Eq, Ord, Num, Fractional, RealFrac, Real)
-}
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
  fishTypeMap :: HashMap Vertex (Set FishType),
  edgeCostMap :: HashMap (Vertex, Vertex) Time,
  adjacencyMap :: HashMap Vertex [Vertex]
  -- ans :: Time
 }

example = Test {
  numNodes = 5,
  numFishTypes = 3,
  fishTypeMap = [(1,[1]),(2,[2]),(3,[2,3]),(4,[2]),(5,[2])],
  edgeCostMap = let 
      undirectedEdgeCostList = [((1,2),10),((1,3),15),((1,4),1),((3,5),5)]
    in HM.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
  adjacencyMap = toAdjacencyMap (HM.keys (edgeCostMap example)) --, ans = Time 20
}
sample = Test {
  numNodes = 5,
  numFishTypes = 5,
  fishTypeMap = [(1,[1]),(2,[1,2]),(3,[3]),(4,[4]),(5,[5])],
  edgeCostMap = let 
      undirectedEdgeCostList = [((1,2),10), ((1,3),10), ((2,4),10), ((3,5),10), ((4,5),10)]
    in HM.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
  adjacencyMap = toAdjacencyMap (HM.keys (edgeCostMap sample)) --, ans = Time 30
}

toAdjacencyMap edges = HM.fromListWith (++) $ L.map (\(a,b) -> (a,b:[])) edges

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
      startFishTypes = foldl' f zeroBits $ maybe (error "no startFishTypes") id $ HM.lookup start fishTypeMap 
      f :: Integer -> Int -> Integer
      f b a = b .|. bit (a-1)
      s = Opt 0 start startFishTypes
      initialQueue = singleton 0 s
  state <- A.newArray ((1,0),(numNodes,universalSet)) (1/0)
  A.writeArray state (start,startFishTypes) 0 
  go state initialQueue 
  where
    go state (minView -> Nothing) = pure state
    go state (minView -> Just ((Opt t u mask), pq)) = do
      pq' <- foldM step pq adjs
      go state pq'
      where
        adjs = fromMaybe [] $ HM.lookup u adjacencyMap
        step queue v = do 
          let !mask' = mask .|. vFishTypes
              !tmp = t + cost
          orig <- A.readArray state (v,mask')
          case (tmp < orig) of
               True -> do
                 A.writeArray state (v,mask') tmp
                 pure accQueue
                 where 
                   sourceOpt = Opt tmp v mask'
                   accQueue = insert tmp sourceOpt queue
               False -> do
                 pure queue
          where
            cost = fromMaybe (error "no cost") $ HM.lookup (u,v) edgeCostMap 
            vFishTypes = foldl' f zeroBits $ fromMaybe (error "no vFishTypes") $ HM.lookup v fishTypeMap
            f :: Integer -> Int -> Integer
            f b a = b .|. bit (a-1)
            

