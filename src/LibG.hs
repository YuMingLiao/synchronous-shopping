module LibG where
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G
import qualified Data.Map as M 
import qualified Data.List as L
import Data.Tuple (swap)
type Vertex = Int
type VLabel = [FishType]
type FishType = Int
type ELabel = Int
shopGraphParams :: G.GraphvizParams Vertex VLabel ELabel () VLabel
shopGraphParams = G.defaultParams {
  G.isDirected = False,
  G.fmtNode = \(v, vl) -> [], 
  G.fmtEdge = \(from, to, el) -> [] 
}  
read' = read . TL.unpack
main = do
  input <- TL.getContents
  let dotText = processInput input
  TL.writeFile "input18.dot" dotText
    where
      processInput input = dotText
       where
        dotText = G.printDotGraph dotGraph :: TL.Text
        dotGraph = G.graphElemsToDot shopGraphParams vs es :: G.DotGraph Vertex
        inputLines = TL.lines input
        (firstLine, rest) = L.splitAt 1 inputLines
        firstLine' = L.map read' . TL.words $ head firstLine
        n = firstLine' !! 0
        k = firstLine' !! 2
        (cityLines, edgeLines) = L.splitAt n rest
        fishTypeMap = M.fromList . zipWith (,) [1..n] . L.map (L.drop 1 . L.map read') . L.map TL.words $ cityLines
        undirectedEdgeCostList = L.map (\(u:v:c:[]) -> ((u,v),c)) . L.map (L.map read' . TL.words) $ edgeLines
        -- edgeCostMap = let 
        --    undirectedEdgeCostList = L.map (\(u:v:c:[]) -> ((u,v),c)) . L.map (L.map read' . TL.words) $ edgeLines
        --    in M.fromList $ undirectedEdgeCostList ++ L.map (\(e,t) -> (swap e, t)) undirectedEdgeCostList
        vs = M.toList fishTypeMap
        es = (\((a,b),c) -> (a,b,c)) <$> undirectedEdgeCostList 
 
{-
main :: IO ()
main = do
  -- 1. Create our application-specific graph
  (vs, es) <- readDirectoryGraph rootDir
  -- 2. Convert it into a DotGraph
  let dotGraph = G.graphElemsToDot fileGraphParams vs es :: G.DotGraph FilePath
  -- 3. Render it into .dot text
      dotText = G.printDotGraph dotGraph :: TL.Text
  -- 4. Write the contents to a file
  
writeDotFile dotText = TL.writeFile "files.dot" dotText
-}
