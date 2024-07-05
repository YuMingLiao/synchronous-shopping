{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import Prelude hiding (foldl)
import Data.Monoid
import qualified Data.List as L
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M hiding (insertLookupWithKey)
import qualified Data.Map.Strict as M (insertLookupWithKey)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Tuple (swap)
import Data.Text.Lazy (unpack)
import Data.Maybe (fromJust)
import Control.Exception (assert)
import Data.Ord (comparing)
import Data.Hashable
import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import Data.Foldable (foldl)
import Data.Bits

{- Hackerrank needs it.
instance Hashable v => Hashable (S.Set v) where
    hashWithSalt salt x = S.foldl' hashWithSalt (hashWithSalt salt (S.size x)) x 
-}
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
    processInput input = show $ findShortestTwoPaths testData 
      where
        inputLines = lines input
        (firstLine, rest) = L.splitAt 1 inputLines
        firstLine' = L.map read . words $ head firstLine
        n = firstLine' !! 0
        k = firstLine' !! 2
        (cityLines, edgeLines) = L.splitAt n rest
        testData = Test {
          numNodes = n,
          numFishTypes = toInteger k,
          fishTypeMap = HM.fromList . zipWith (,) [1..n] . L.map (S.fromList . L.drop 1 . L.map read) . L.map words $ cityLines,
          edgeCostMap = let 
            undirectedEdgeCostList = L.map (\(u:v:c:[]) -> ((u,v),Time (fromIntegral c))) . L.map (L.map read . words) $ edgeLines
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
debugHere s a | debugFlag = Debug.Trace.trace s a
              | otherwise = a
debugWhen pred | pred && debugFlag == True = debug
               | otherwise = \_ _ b -> b 


newtype Time = Time {
  unTime :: Float
} deriving (Eq, Ord, Num, Fractional, RealFrac, Real)
instance Show Time where
  show (Time t) | t == 1/0 = "Infinity"
                | otherwise =  show $ round t
instance Semigroup Time where
  a <> b | a <= b = a
  a <> b | a >  b = b 

data Path = Path {
  path :: [Vertex],
  time :: Time
}

data SourceOpt = Opt {
  timeUsed :: Time,
  source :: Vertex
} deriving Eq 
instance Show SourceOpt where
  show (Opt t s) | s == 166 || s == 820 || s == 928 = show (t,s)
                 | otherwise = show (t,s)

instance Show Path where
  show (Path path time) = show (time, path)
instance Eq Path where
  (==) (Path _ t1) (Path _ t2) = t1 == t2
instance Ord Path where
  compare (Path _ t1) (Path _ t2) = compare t1 t2

instance Ord SourceOpt where
  compare (Opt t1 v1) (Opt t2 v2) | compare t1 t2 == EQ = compare v1 v2
                                  | otherwise = compare t1 t2
 
    where eiShop v1 v2 =v1 `elem` ([166,820,928] :: [Vertex]) || v2 `elem` ([166,820,928] :: [Vertex])
instance {-# OVERLAPPING #-} Show a => Show (HashMap (Set FishType) a) where
  show m = show $ L.map (\(a,b) -> (S.toList a,b)) $ HM.toList m 

instance {-# OVERLAPPING #-} Show (Set FishType) where
  show s = show $ S.toList s
 
instance {-# OVERLAPPING #-} Show (HashMap Vertex NodeState) where
  show m = show $ HM.toList m


type FishType = Integer
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
    sortedList = L.sortOn ((\(Path _ t) -> t) . snd) $ A.assocs finalState
    completions = L.filter f [(x,y) |  x <- sortedList, y <- sortedList]
    f ((s1,_),(s2,_)) = (s1 .|. s2) == numFishTypes - 1
    go sortedList = (\((_,Path _ t1),(_,Path _ t2))-> max t1 t2) $ head $ L.sortOn sortFunc completions
    sortFunc ((_,Path _ t1),(_,Path _ t2)) = max t1 t2 

findFinalState :: Test -> Array Combination Path
findFinalState testData@(Test {..}) = 
 maybe (error "no final state") id $ HM.lookup numNodes $ dijkstra testData 1

type Combination = Integer -- binary    
type NodeState = Array Combination Path 

dijkstra (Test {..}) start = go initialStateMap initialQueue 
  where
    numCombination :: Integer
    numCombination = debugId "numCombination" $ 2^numFishTypes
    nodeStateDef :: NodeState 
    nodeStateDef = debugHere "nodeStateDef" $ A.listArray (0, debugId "numCombination-1" $ numCombination-1) $ debugHere "repeat" $ repeat (Path [] (Time (1/0)))
    
    startState = debugHere "startStaet" $ nodeStateDef // [startFishState] 
      where
        startFishTypes :: Integer
        startFishTypes = foldl f zeroBits $ maybe (error "no startFishTypes") id $ HM.lookup start fishTypeMap 
        f :: Integer -> Integer -> Integer
        f b a =  bit b .|. bit a
        startFishState = debugHere "startFishState" $ (startFishTypes, (Path [start] (Time 0))) 
    initialStateMap = debugHere "initialStateMap" $ HM.update (const (Just startState)) start $ HM.fromList $ zipWith (,) [1..numNodes] (repeat nodeStateDef)
    initialQueue = singleton (Time 0) s
      where s = Opt (Time 0) start
    go :: HashMap Vertex NodeState -> PriorityQueue Time SourceOpt -> HashMap Vertex NodeState
    go state (minView -> Nothing) = state
    go state (minView -> Just ((Opt t u), rest)) | debug "u" u True = go state' queue'
      where
        (state', queue') = L.foldl f (state,rest) adjs
        f (state, queue) v | debug "v" v True = 
          case genState state u v cost of
               Just vState' -> (state', accQueue)
                 where 
                   state' = HM.update (const (Just vState')) v state
                   sourceOpt = Opt (t + cost) v
                   accQueue = insert (t + cost) sourceOpt queue
               Nothing -> (state, queue)
          where
            cost = maybe (error "no cost") id $ HM.lookup (u,v) edgeCostMap 
        adjs = maybe [] id $ HM.lookup u adjacencyMap
        genState state u v cost | debugHere "genState" True = 
          case debugHere "isWorthStepping" isWorthStepping of 
               True  -> Just vState' 
               False -> Nothing
           where
            uState = debugHere "uState" $ maybe (error "no uState") id $ HM.lookup u state
            vState = debugHere "vState" $ maybe (error "no vState") id $ HM.lookup v state
            vFishTypes =  foldl f zeroBits $ maybe (error "no vFishTypes") id $ HM.lookup v fishTypeMap
            f b a = bit b .|. bit a
            (isWorthStepping, vState') = debugHere "foldl" $ foldl (debugHere "enter f" foldFunc) (False, debugHere "vState" vState) $ A.assocs $ debugId "uState" uState
              where
                foldFunc (isUpdated, acc) (k, path@(Path p a)) | debugHere "foldFunc" True = 
                  if | a /= 1/0 -> let 
                           (k', path'@(Path p' a')) = ((k .|. vFishTypes), (Path (v:p) (a + cost)))
                           origSolution = debugId "acc ! k'" $ acc ! k' 
                           shouldUpdate =
                                    case compare origSolution path' of
                                         LT -> False
                                         EQ -> False
                                         GT -> True
                           acc' | shouldUpdate == True = acc // [(k', path')]
                                | otherwise = acc
                           isUpdated' = isUpdated || shouldUpdate
                           in (isUpdated', acc')
                             
                     | otherwise -> (isUpdated, acc)

-- TODO: [x] NodeState needs to be MonoidalMap. 
--       [x] And Time needs to be a newtype with customized <>.
--       [x] gonna need a function (foldMapWithKey) of uState -> (vFishTypes, t) -> vState' 
--       [x] if vState' updates vState then put v into Q, otherwise don't.
--       [] hackerrank uses multiset instead of heap or piority queue
--       [] hackerrank uses Map (unionWith) insead of monoidal-containers 
--function Dijkstra(Graph, source):
--
--      for each vertex v in Graph.Vertices:
--          dist[v] <- INFINITY
--          add v to Q
--      dist[source] <- 0
--
--      while Q is not empty:
--          u <- vertex in Q with min dist[u]
--
--          for each neighbor v of u:
--              alt <- dist[u] + Graph.Edges(u, v)
--              if alt < dist[v] and dist[u] is not INFINITY:
--                  dist[v] <- alt
--                  put v into Q.
--          
--      return dist[]
