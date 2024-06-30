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
import Data.Map
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.Text.Lazy (unpack)
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
          fishTypeMap = M.fromList . zipWith (,) [1..n] . L.map (S.fromList . L.drop 1 . L.map read) . L.map words $ cityLines,
          edgeCostMap = let 
            undirectedEdgeCostList = L.map (\(u:v:c:[]) -> ((u,v),Time (fromIntegral c))) . L.map (L.map read . words) $ edgeLines
            in M.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
          adjacencyMap = toAdjacencyMap (M.keys (edgeCostMap testData))
        } 
 
-- print $ findShortestTwoPaths example 

debugFlag = False
debug s a b | debugFlag = Debug.Trace.trace (s ++ ": " ++ show a) b 
            | otherwise = b   
debugId s a | debugFlag = Debug.Trace.trace (s ++ ": " ++ show a) a 
            | otherwise = a   



newtype Time = Time {
  unTime :: Float
} deriving (Eq, Ord, Num, Fractional, RealFrac, Real)
instance Show Time where
  show (Time t) | t == 1/0 = show t
                | otherwise =  show $ round t
instance Semigroup Time where
  a <> b | a <= b = a
  a <> b | a >  b = b 

data Path = Path {
  path :: [Vertex],
  time :: Time
}

data SourceOpt = Opt {
  time :: Time,
  source :: Vertex
} 


instance Show Path where
  show (Path path time) = show (time, path)
instance Eq Path where
  (==) (Path _ t1) (Path _ t2) = t1 == t2
instance Ord Path where
  compare (Path _ t1) (Path _ t2) = compare t1 t2

instance Eq SourceOpt where
  (==) (Opt t1 _) (Opt t2 _) = t1 == t2
instance Ord SourceOpt where
  compare (Opt t1 _) (Opt t2 _) = compare t1 t2

instance {-# OVERLAPPING #-} Show (M.Map (Set FishType) Time) where
  show m = show $ L.map (\(a,b) -> (S.toList a,b)) $ M.toList m 

type FishType = Int
type Vertex = Int

data Test = Test {
  numNodes :: Int,
  numFishTypes :: FishType,
  fishTypeMap :: M.Map Vertex (Set FishType),
  edgeCostMap :: M.Map (Vertex, Vertex) Time,
  adjacencyMap :: M.Map Vertex [Vertex],
  ans :: Time
 }

example = Test {
  numNodes = 5,
  numFishTypes = 3,
  fishTypeMap = [(1,[1]),(2,[2]),(3,[2,3]),(4,[2]),(5,[2])],
  edgeCostMap = let 
      undirectedEdgeCostList = [((1,2),10),((1,3),15),((1,4),1),((3,5),5)]
    in M.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
  adjacencyMap = toAdjacencyMap (M.keys (edgeCostMap example)), 
  ans = Time 20
}
sample = Test {
  numNodes = 5,
  numFishTypes = 5,
  fishTypeMap = [(1,[1]),(2,[1,2]),(3,[3]),(4,[4]),(5,[5])],
  edgeCostMap = let 
      undirectedEdgeCostList = [((1,2),10), ((1,3),10), ((2,4),10), ((3,5),10), ((4,5),10)]
    in M.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
  adjacencyMap = toAdjacencyMap (M.keys (edgeCostMap sample)),
  ans = Time 30
}

toAdjacencyMap edges = M.fromListWith (++) $ L.map (\(a,b) -> (a,b:[])) edges



findShortestTwoPaths testData@(Test {..}) = go sortedList 
  where
    finalState = debugId "finalState: " $ findFinalState testData
    sortedList = L.sortOn ((\(Path _ t) -> t) . snd) $ M.toList finalState
    completions = L.filter f [(x,y) |  x <- sortedList, y <- sortedList]
    f ((s1,_),(s2,_)) = (s1 `S.union` s2) == (S.fromList [1..numFishTypes])
    go sortedList = (\((_,Path _ t1),(_,Path _ t2))-> max t1 t2) $ head $ L.sortOn sortFunc completions
    sortFunc ((_,Path _ t1),(_,Path _ t2)) = max t1 t2 

findFinalState :: Test -> M.Map (Set FishType) Path
findFinalState testData@(Test {..}) = 
 maybe (error "no final state") id $ M.lookup numNodes $ dijkstra testData 1

    
type NodeState = M.Map (Set FishType) Path

dijkstra (Test {..}) start = go initialStateMap initialQueue 
  where
    nodeStateDef :: NodeState 
    nodeStateDef = M.fromList $ L.map (,Path [] (Time (1/0))) $ S.toList $ S.powerSet [1..numFishTypes] 
    startState = mconcat (startFishState:nodeStateDef:[]) 
      where
        startFishTypes = maybe (error "no startFishTypes") id $ M.lookup start fishTypeMap 
        startFishState = M.singleton startFishTypes (Path [start] (Time 0)) 
    initialStateMap = M.update (const (Just startState)) start $ M.fromList $ zipWith (,) [1..numNodes] (repeat nodeStateDef)
    initialQueue = M.singleton s s
      where s = Opt (Time 0) start
    go :: M.Map Vertex NodeState -> M.Map SourceOpt SourceOpt -> M.Map Vertex NodeState
    go state (M.minView -> Nothing) = state
    go state (M.minView -> Just ((Opt t u), rest)) = go state' queue'
      where
        (state', queue') {- | debugL (("u",u):("adjacencyMap",adjacencyMap):("adjs",adjs):[]) True -} = L.foldl f (state,rest) adjs
        f (state, rest) v = 
          case debug "v" v $ genState state u v cost of
               Just vState' -> debug "update state'" vState' $ (M.update (const (Just vState')) v state, M.insert sourceOpt sourceOpt rest)
                 where sourceOpt = Opt (t + cost) v
               Nothing -> debug "no update" "" (state, rest)
          where
            cost = maybe (error "no cost") id $ M.lookup (u,v) edgeCostMap 
        adjs = maybe [] id $ M.lookup u adjacencyMap
        genState state u v cost = 
          case debug "uState" uState $ debug "vState" vState $ debug "altState" altState $ debug "vState'" vState' $ isWorthStepping of 
               True  -> Just vState' 
               False -> Nothing
          where
            uState = maybe (error "no uState") id $ M.lookup u state
            vState = maybe (error "no vState") id $ M.lookup v state
            vFishTypes = maybe (error "no vFishTypes") id $ M.lookup v fishTypeMap
            altState = M.foldlWithKey foldlFunc M.empty uState
              where 
                foldlFunc acc k (Path p a) | debug "acc" acc $ debug "k" k $ debug "a" a $ True =
                  if | a /= 1/0 -> let vStep = M.singleton (k `S.union` vFishTypes) (Path (v:p) (a + cost))
                                   in M.unionWithKey unionFunc vStep acc
                     | otherwise -> acc 
            vState' = M.unionWithKey unionFunc altState vState
            unionFunc k a b = min a b
            isWorthStepping = 
              if | vState' == vState -> False
                 | otherwise -> True 

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
