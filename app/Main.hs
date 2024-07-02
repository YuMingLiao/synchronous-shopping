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
import Data.Maybe (fromJust)
import Control.Exception (assert)
import Data.Ord (comparing)
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
instance {-# OVERLAPPING #-} Show a => Show (M.Map (Set FishType) a) where
  show m = show $ L.map (\(a,b) -> (S.toList a,b)) $ M.toList m 

instance {-# OVERLAPPING #-} Show (Set FishType) where
  show s = show $ S.toList s
 
instance {-# OVERLAPPING #-} Show (M.Map Vertex NodeState) where
  show m = show $ M.toList m

instance {-# OVERLAPPING #-} Show (M.Map SourceOpt SourceOpt) where
  show m = show . keys $ M.filterWithKey f m
    where f (Opt t v) a = v == 166 || v == 820 || v == 928 

type FishType = Int
type Vertex = Int

data Test = Test {
  numNodes :: Int,
  numFishTypes :: FishType,
  fishTypeMap :: M.Map Vertex (Set FishType),
  edgeCostMap :: M.Map (Vertex, Vertex) Time,
  adjacencyMap :: M.Map Vertex [Vertex]
  -- ans :: Time
 }

example = Test {
  numNodes = 5,
  numFishTypes = 3,
  fishTypeMap = [(1,[1]),(2,[2]),(3,[2,3]),(4,[2]),(5,[2])],
  edgeCostMap = let 
      undirectedEdgeCostList = [((1,2),10),((1,3),15),((1,4),1),((3,5),5)]
    in M.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
  adjacencyMap = toAdjacencyMap (M.keys (edgeCostMap example)) --, ans = Time 20
}
sample = Test {
  numNodes = 5,
  numFishTypes = 5,
  fishTypeMap = [(1,[1]),(2,[1,2]),(3,[3]),(4,[4]),(5,[5])],
  edgeCostMap = let 
      undirectedEdgeCostList = [((1,2),10), ((1,3),10), ((2,4),10), ((3,5),10), ((4,5),10)]
    in M.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList,
  adjacencyMap = toAdjacencyMap (M.keys (edgeCostMap sample)) --, ans = Time 30
}

toAdjacencyMap edges = M.fromListWith (++) $ L.map (\(a,b) -> (a,b:[])) edges

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y =
  case comparing f x y of
    LT -> x
    EQ -> x
    _ -> y


findShortestTwoPaths testData@(Test {..}) = go sortedList 
  where
    finalState = findFinalState testData
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
    go state (M.minView -> Just ((Opt t u), rest)) | debug "u" u $ debug "rest" rest True = go state' queue'Asserted
      where
        uIsShop = u == 166 || u == 820 || u == 928
        queue'Asserted = assert shopPassedRight queue'
        shopPassedRight = (if
              | 166 `member` rest -> 166 `member` queue'
              | otherwise -> True)
          ||
          (if | 820 `member` rest -> 820 `member` queue'
              | otherwise -> True)
          ||
          (if | 928 `member` rest -> 928 `member` queue'
              | otherwise -> True)
        member shop q = M.filter (\(Opt _ v)->(v==shop)) q /= M.empty

        (state', queue') = L.foldl f (state,rest) adjs
        f (state, queue) v | debug "v" v True = 
          case genState state u v cost of
               Just vState' -> (state', debugId "accQueueAsserted" accQueueAsserted)
                 where 
                   state' = M.update (const (Just vState')) v state
                   sourceOpt = Opt (t + cost) v
                   accQueue = M.insert sourceOpt sourceOpt queue
                   accQueueAsserted = assert shopPassedRight accQueue
                   shopPassedRight = (if
                     | 166 `member` queue -> 166 `member` accQueue
                     | otherwise -> True)
                     ||
                     (if | 820 `member` queue -> 820 `member` accQueue
                         | otherwise -> True)
                     ||
                     (if | 928 `member` queue -> 928 `member` accQueue
                         | otherwise -> True)
                   member shop q = M.filter (\(Opt _ v)->(v==shop)) q /= M.empty
               Nothing -> (state, queue)
          where
            cost = maybe (error "no cost") id $ M.lookup (u,v) edgeCostMap 
        adjs = maybe [] id $ M.lookup u adjacencyMap
        genState state u v cost = 
          case isWorthStepping of 
               True  -> Just vState' 
               False -> Nothing
           where
            uState = maybe (error "no uState") id $ M.lookup u state
            vState = maybe (error "no vState") id $ M.lookup v state
            vFishTypes = maybe (error "no vFishTypes") id $ M.lookup v fishTypeMap
            vState' = M.foldrWithKey foldFunc (vState, False) uState
              where
                foldFunc k path@(Path p a) (acc, improved) = 
                  if | a /= 1/0 -> let 
                           (k', path'@(Path p' a')) = ((k `S.union` vFishTypes), (Path (v:p) (a + cost)))

                           alterIfFaster k m = M.alter alterFunc k' acc
                             where
                               alterFunc Nothing = Just path'
                               alterFunc (Just path@(Path p a)) = Just (minOn time path path')   
				isFaster =  
                         in M.alter alterFunc k' acc
                     | otherwise -> acc
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
