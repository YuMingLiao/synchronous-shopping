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
import qualified Data.HashTable.ST.Basic as H
import qualified Data.HashTable.Class as H (toList, fromList)
import qualified Data.HashTable.IO as H (BasicHashTable)
import Control.Monad.ST
import Control.Monad (foldM)

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
          numFishTypes = k,
          fishTypeMap = HM.fromList . zipWith (,) [1..n] . L.map (S.fromList . L.drop 1 . L.map read) . L.map words $ cityLines,
          edgeCostMap = let 
            undirectedEdgeCostList = L.map (\(u:v:c:[]) -> ((u,v),Time (fromIntegral c))) . L.map (L.map read . words) $ edgeLines
            in HM.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
          adjacencyMap = toAdjacencyMap (HM.keys (edgeCostMap testData))
        } 
 
-- print $ findShortestTwoPaths example 

debugFlag = False
debug s a b | debugFlag = Debug.Trace.trace (s ++ ": " ++ show a) b 
            | otherwise = b   
debugId s a | debugFlag = Debug.Trace.trace (s ++ ": " ++ show a) a 
            | otherwise = a   
debugView s f a | debugFlag = Debug.Trace.trace (s ++ ": " ++ show (f a)) a 
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
    finalState = runST $ findFinalState testData
    sortedList = L.sortOn ((\(Path _ t) -> t) . snd) $ HM.toList finalState
    completions = L.filter f [(x,y) |  x <- sortedList, y <- sortedList]
    f ((s1,_),(s2,_)) = (s1 `S.union` s2) == (S.fromList [1..numFishTypes])
    go sortedList = (\((_,Path _ t1),(_,Path _ t2))-> max t1 t2) $ head $ L.sortOn sortFunc completions
    sortFunc ((_,Path _ t1),(_,Path _ t2)) = max t1 t2 

findFinalState :: Test -> ST s (HashMap (Set FishType) Path)
findFinalState testData@(Test {..}) = do 
  r <- dijkstra testData 1
  let finalStateHT = maybe (error "no final state") id $ HM.lookup numNodes
  finalStateHM <- HM.fromList <$> H.toList finalStateHT
  pure finalStateHM 

    
type NodeState = H.BasicHashTable (Set FishType) Path

dijkstra (Test {..}) start = do
  let indivNodeState = HM.fromList $ L.map (,Path [] (Time (1/0))) $ allFishTypesCombination
      startFishTypes = maybe (error "no startFishTypes") id $ HM.lookup start fishTypeMap 
      startNodeState = HM.insert startFishTypes (Path [start] (Time 0)) indivNodeState  
      nodeState = HM.fromList $ zipWith (,) [1..numNodes] $ take numNodes $ repeat indivNodeState 
      nodeState' = HM.insert nodeState start startNodeState 
      initalState = mapM mkIndivNodeStateHT startNodeState' 
      mkIndivNodeStateHT a = H.fromList $ HM.toList a
  go initialState initialQueue 
  where
    initialQueue = singleton (Time 0) s
      where s = Opt (Time 0) start
    go :: HashMap Vertex NodeState -> PriorityQueue Time SourceOpt -> HashMap Vertex NodeState
    go state (minView -> Nothing) = state
    go state (minView -> Just ((Opt t u), rest)) | debug "u" u True = go state' queue'
      where
        (state', queue') = foldM f (state,rest) adjs
        f (state, queue) v | debug "v" v True = 
          case genState state u v cost of
               Just vState' -> do
                 pure (accState, accQueue)
                 where 
                   accState = HM.update (const (Just vState')) v state
                   sourceOpt = Opt (t + cost) v
                   accQueue = insert (t + cost) sourceOpt queue
               Nothing -> (state, queue)
          where
            cost = maybe (error "no cost") id $ HM.lookup (u,v) edgeCostMap 
        adjs = maybe [] id $ HM.lookup u adjacencyMap
        genState state u v cost = do
          (isWorthStepping, vState') <- foldM foldFunc (False, vState) uState
          case isWorthStepping of 
               True  -> pure $ Just vState' 
               False -> pure Nothing
           where
            uState = maybe (error "no uState") id $ HM.lookup u state
            vState = maybe (error "no vState") id $ HM.lookup v state
            vFishTypes = maybe (error "no vFishTypes") id $ HM.lookup v fishTypeMap
            foldFunc (isUpdated, acc) (k, path@(Path p a)) =  
              if | a /= 1/0 -> do
                   let (k', path'@(Path p' a')) = ((k `S.union` vFishTypes), (Path (v:p) (a + cost)))
                   maybeExist <- H.lookup k' acc
                   let shouldUpdate = case maybeExist of 
                                        Nothing -> True
                                        (Just origSolution) -> case compare origSolution path' of
                                                                 LT -> False
                                                                 EQ -> False
                                                                 GT -> True
                   let acc' | shouldUpdate == True = HM.insert k' path' acc
                            | otherwise = acc
                   let isUpdated' = isUpdated || shouldUpdate
                   pure (isUpdated', acc')
                             
                 | otherwise -> pure (isUpdated, acc)

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
