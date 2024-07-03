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
import qualified Data.Map as M hiding (insertLookupWithKey)
import qualified Data.Map.Strict as M (insertLookupWithKey)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict as HM
import Data.Tuple (swap)
import Data.Text.Lazy (unpack)
import Data.Maybe (fromJust)
import Control.Exception (assert)
import Data.Ord (comparing)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS

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
 
instance {-# OVERLAPPING #-} Show (HashMap Vertex NodeState) where
  show m = show $ HM.toList m


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
    sortedList = L.sortOn ((\(Path _ t) -> t) . snd) $ HM.toList finalState
    completions = L.filter f [(x,y) |  x <- sortedList, y <- sortedList]
    f ((s1,_),(s2,_)) = (s1 `S.union` s2) == (S.fromList [1..numFishTypes])
    go sortedList = (\((_,Path _ t1),(_,Path _ t2))-> max t1 t2) $ head $ L.sortOn sortFunc completions
    sortFunc ((_,Path _ t1),(_,Path _ t2)) = max t1 t2 

findFinalState :: Test -> HashMap (Set FishType) Path
findFinalState testData@(Test {..}) = 
 maybe (error "no final state") id $ HM.lookup numNodes $ dijkstra testData 1

    
type NodeState = HashMap (Set FishType) Path

dijkstra (Test {..}) start = go initialStateMap initialQueue 
  where
    nodeStateDef :: NodeState 
    nodeStateDef = HM.fromList $ L.map (,Path [] (Time (1/0))) $ S.toList $ S.powerSet [1..numFishTypes] 
    startState = mconcat (startFishState:nodeStateDef:[]) 
      where
        startFishTypes = maybe (error "no startFishTypes") id $ HM.lookup start fishTypeMap 
        startFishState = HM.singleton startFishTypes (Path [start] (Time 0)) 
    initialStateMap = HM.update (const (Just startState)) start $ HM.fromList $ zipWith (,) [1..numNodes] (repeat nodeStateDef)
    initialQueue = MS.singleton s
      where s = Opt (Time 0) start
    go :: HashMap Vertex NodeState -> MultiSet SourceOpt -> HashMap Vertex NodeState
    go state (MS.minView -> Nothing) = state
    go state (MS.minView -> Just ((Opt t u), rest)) | debug "u" u $ debug "rest" rest True = go state' queue'Asserted
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
        member shop q = MS.filter (\(Opt _ v)->(v==shop)) q /= MS.empty

        (state', queue') = L.foldl f (state,rest) adjs
        f (state, queue) v | debug "v" v True = 
          case genState state u v cost of
               Just vState' -> (state', debugId "accQueueAsserted" accQueueAsserted)
                 where 
                   state' = HM.update (const (Just vState')) v state
                   sourceOpt = Opt (t + cost) v
                   accQueue = MS.insert sourceOpt queue
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
                   member shop q = MS.filter (\(Opt _ v)->(v==shop)) q /= MS.empty
               Nothing -> (state, queue)
          where
            cost = maybe (error "no cost") id $ HM.lookup (u,v) edgeCostMap 
        adjs = maybe [] id $ HM.lookup u adjacencyMap
        genState state u v cost = 
          case isWorthStepping of 
               True  -> Just vState' 
               False -> Nothing
           where
            uState = maybe (error "no uState") id $ HM.lookup u state
            vState = maybe (error "no vState") id $ HM.lookup v state
            vFishTypes = maybe (error "no vFishTypes") id $ HM.lookup v fishTypeMap
            (isWorthStepping, vState') = HM.foldrWithKey foldFunc (False, vState) uState
              where
                foldFunc k path@(Path p a) (isUpdated, acc) = 
                  if | a /= 1/0 -> let 
                           (k', path'@(Path p' a')) = ((k `S.union` vFishTypes), (Path (v:p) (a + cost)))
                           maybeExist = HM.lookup k' acc
                           shouldUpdate =
                             case maybeExist of 
                                  Nothing -> True
                                  (Just origSolution) -> 
                                    case compare origSolution path' of
                                         LT -> False
                                         EQ -> False
                                         GT -> True
                           acc' | shouldUpdate == True = HM.insert k' path' acc
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
