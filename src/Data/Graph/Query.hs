module Data.Graph.Query ( Tree(..)
                        , bfs
                        , bft
                        , dff
                        , dijkstra
                        , esp
                        , getLPath
                        , getPath
                        , postorder
                        , scc
                        , spd
                        , spdDist
                        , spdPath
                        , spt
                        , topsort
                        , dfs
                        ) where

import           Data.Graph
import qualified Data.Heap  as H
import           Data.List
import           Safe       (headMay)

-- | Perform a depth-first search on a graph, the first parameter is a list of
-- possibly unvisited nodes.
--
-- TODO Using lists as queues is inefficient.
dfs :: Graph gr => [Node] -> gr a b -> [Node]
dfs [] _            = []
dfs _ g | isEmpty g = []
dfs (v:vs) g        = case match v g of
                        (Just c, g') -> v:dfs (suc c ++ vs) g'
                        (Nothing, _) -> dfs vs g

-- | Perform a breadth-first search on a graph, the first parameter is a list of
-- possibly unvisited nodes.
--
-- TODO Using lists as queues is inefficient.
bfs :: Graph gr => [Node] -> gr a b -> [Node]
bfs [] _            = []
bfs _ g | isEmpty g = []
bfs (v:vs) g        = case match v g of
                        (Just c, g') -> v:bfs (vs ++ suc c) g'
                        (Nothing, _) -> bfs vs g

data Tree a = Br a [Tree a]
  deriving Show

-- | Postorder traversal of a tree.
postorder :: Tree a -> [a]
postorder (Br v ts) = concatMap postorder ts++[v]

-- | Compute a depth-first spanning forest.
--
-- TODO Using lists as queues is inefficient.
dff :: Graph gr => [Node] -> gr a b -> [Tree Node]
dff ns gr = fst (df ns gr)
  where
    df :: Graph gr => [Node] -> gr a b -> ([Tree Node], gr a b)
    df [] g            = ([], g)
    df _ g | isEmpty g = ([], g)
    df (v:vs) g        = case match v g of
      -- Create two spanning forests:
      --   + f : for all successors of v
      --   + f': for all remaining nodes to be visited
      (Just c, g') -> (Br v f:f', g2) where (f, g1)  = df (suc c) g'
                                            (f', g2) = df vs g1
      (Nothing, _) -> df vs g

-- | Topologically sort a graph.
topsort :: Graph gr => gr a b -> [Node]
topsort g = reverse . concatMap postorder . dff (nodes g) $ g

-- | Compute strongly connected components.
scc :: Graph gr => gr a b -> [Tree Node]
scc g = dff (topsort g) (grev g)

type Path = [Node]
-- | Path to root of tree.
type RTree = [Path]

-- | Compute a breadth-first spanning tree.
--
-- TODO Using lists as queues is inefficient.
bft :: Graph gr => Node -> gr a b -> RTree
bft n = bf [[n]]
  where
    bf :: Graph gr => [Path] -> gr a b -> RTree
    bf [] _             = []
    bf ([]:_) _         = []
    bf _ g | isEmpty g  = []
    bf (p@(v:_) : ps) g = case match v g of
      (Just c, g') -> p:bf (ps ++ map rootPath (suc c)) g' where rootPath = (:p)
      (Nothing, _) -> bf ps g

firstWhere :: (a -> Bool) -> [a] -> Maybe a
firstWhere p = headMay . filter p

-- | Compute the shortest path between two nodes in a graph, ignoring edge
-- labels.
esp :: Graph gr => Node -> Node -> gr a b -> Maybe Path
esp s t g = do
  let tree = bft s g -- anchored at s
  rpath <- firstWhere (\(v:_) -> v == t) tree
  pure $ reverse rpath

-- Computing the shortest path when the edges have an associated "cost" is very
-- similar to the computing the shortest-path using a breadth-first search, only
-- we don't keep the root paths in a tree but in a heap that is ordered by the
-- lengths of the paths.
type LPath a  = [LNode a]
type LRTree a = [LPath a]

-- | Find first path that starts at the given node.
getPath :: Node -> LRTree a -> Maybe Path
getPath v = fmap (reverse . map fst) . firstWhere (\((w, _):_) -> w == v)

-- | Same as `getPath` but also includes the labels of each path.
getLPath :: Node -> LRTree a -> Maybe (LPath a)
getLPath v = fmap (reverse) . firstWhere (\((w, _):_) -> w == v)

expand :: Real b => b -> LPath b -> Context a b -> [H.Heap b (LPath b)]
expand d p (_, _, _, s) = map (\(l, v) -> H.unit (l + d) ((v, l + d):p)) s

dijkstra :: (Real b, Graph gr) => H.Heap b (LPath b) -> gr a b -> LRTree b
dijkstra h g | H.isEmpty h || isEmpty g = []
dijkstra h g =
  case match v g of
    (Just c, g')  -> p:dijkstra (H.mergeAll (h':expand d p c)) g'
    (Nothing, g') -> dijkstra h' g'
  where (_, p@((v, d):_), h') = H.splitMin h

-- | Compute shortest path tree using Dijkstra's algorithm.
spt :: (Real b, Graph gr) => Node -> gr a b -> LRTree b
spt v = dijkstra (H.unit 0 [(v, 0)])

-- | Compute distance and shortest path using Dijkstra's algorithm.
-- Returns Nothing if there is no path between the two given nodes.
spd :: (Real b, Graph gr) => Node -> Node -> gr a b -> Maybe (b, Path)
spd s t g = do
  let tree = spt s g
  path <- getLPath t tree
  pure $ foldl' (\(_, p) (n, l) -> (l, p ++ [n])) (fromInteger 0, []) path

-- | Compute shortest path using Dijkstra's algorithm.
spdPath :: (Real b, Graph gr) => Node -> Node -> gr a b -> Maybe Path
spdPath s t = fmap snd . spd s t

-- | Compute shortest distance using Dijkstra's algorithm.
spdDist :: (Real b, Graph gr) => Node -> Node -> gr a b -> Maybe b
spdDist s t = fmap fst . spd s t
