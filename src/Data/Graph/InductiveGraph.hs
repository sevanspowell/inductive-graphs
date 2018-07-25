module Data.Graph.InductiveGraph (InductiveGraph) where

import           Data.Graph
import qualified Data.IntMap   as M
import           Safe       (headMay)
import           Data.List
import           Data.Bifunctor (first, second)

-- | Inductive Graph representation.
--
-- Some operations list an amortized running time, `n` refers to the number of
-- entries in the map, `W` refers to the number of bits in an Int (32 or 64) and
-- `d` refers to the degree of a particular node, or the overall degree of nodes
-- in the graph.
newtype InductiveGraph n e = InductiveGraph (GraphRep n e)

instance (Eq n, Eq e, Ord n, Ord e) => Eq (InductiveGraph n e) where
  g1 == g2 =
    (nodes1 == nodes2) && (edges1 == edges2)
      where
        nodes1 = sort $ labelledNodes g1
        nodes2 = sort $ labelledNodes g2
        edges1 = sort $ labelledEdges g1
        edges2 = sort $ labelledEdges g2

-- Internal graph representation using IntMap because our keys (Nodes) are Ints.
type GraphRep n e = M.IntMap (Context' n e)

-- | Context without Node, i.e. (predecessors, node label, successors)
type Context' n e = (Adj e, n, Adj e)

instance (Show n, Show e, Eq e) => Show (InductiveGraph n e) where
  -- Only shows parens if precedence higher than function application
  showsPrec d g = showParen (d > 10) $
    showString "mkGraph "
    . shows (labelledNodes g)
    . showString " "
    . shows (labelledEdges g)

instance Graph InductiveGraph where
  -- | O(1). A empty graph.
  empty = InductiveGraph M.empty

  -- | O(1). Is the graph empty?
  isEmpty (InductiveGraph m) = M.null m

  -- | O(n). List of nodes in the graph and their labels.
  labelledNodes (InductiveGraph g) = do
    (n, (_, l, _)) <- M.toList g
    pure (n, l)

  -- | O(n^2). List of edges in the graph and their labels.
  labelledEdges (InductiveGraph g) = do
    (v, (ps, _, ss)) <- M.toList g
    nub $ concat [(map (\(l, frm) -> (frm, v, l)) ps), (map (\(l,to) -> (v,to,l)) ss)]

  -- | O(min(n, W)). Add a new node (and it's context) to an existing graph.
  (p, n, l, s) & (InductiveGraph g) = InductiveGraph $ M.insert n (p, l, s) g

  -- | O(d*n). Same as match, but matches any node. Note that this is a partial
  -- function but will only fail if used on empty graphs.
  matchAny gr@(InductiveGraph g) = (c, g')
    where
      (c, g') = case (headMay $ M.keys g) of
        Nothing  -> error "Can't matchAny on a graph with no nodes."
        (Just h) -> case match h gr of
          (Nothing, _)   -> error "No context could be obtained."
          (Just c', g'') -> (c', g'')

  -- | O(d*n). Split a graph on the given node, creating a context for the given node as
  -- if it was the last to be added to the graph, as well as the graph without
  -- the given node.
  match v (InductiveGraph g) =
    -- Analyze map and create a list of pred. and succ. entries for any node that
    -- references given node.
    let
      expandContext' (ps, l, ss) = (ps, v, l, ss)

      (c, g') = first (fmap expandContext')
              . M.updateLookupWithKey (\_ _ -> Nothing) v $ g

    in case c of
        Nothing  -> (Nothing, InductiveGraph g')
        (Just (ps, n, l, ss)) ->
          let
            (newPreds, newSuccs, newGraph) = remEdgeRef v g'
          in
            (Just (newPreds ++ ps, n, l, newSuccs ++ ss), InductiveGraph newGraph)

  -- | Make a graph from lists of labelled nodes and labelled edges.
  mkGraph ns es = insEdges es . graphFromNodes $ ns
    where
      graphFromNodes :: [LNode n] -> InductiveGraph n e
      graphFromNodes = InductiveGraph . M.fromList . fmap ( second (\l -> ([], l, [])) )

-- | O(d*n). Remove all edge references to a given node in the graph and
-- return them as a transformed list of predecessors and successors that can be
-- added to the given node's context. Note that the node itself is not removed.
remEdgeRef :: Node -> GraphRep n e -> (Adj e, Adj e, GraphRep n e)
remEdgeRef v g = (ps', ss', g')
  where
    ((ps', ss'), g') = M.mapAccumWithKey f ([], []) g

    f (ps, ss) n c = ((concat [newPs, ps], concat [newSuccs, ss]), c')
      where
        ((newPs, newSuccs), c') = matchAndReturnEdges' v (n, c)

    matchAndReturnEdges' :: Node -> (Node, Context' n e) -> ((Adj e, Adj e), Context' n e)
    matchAndReturnEdges' v' (n, (preds, l, succs)) =
      let
        (newPreds, newSuccs, (preds', _, _, succs')) = matchAndReturnEdges v' (preds, n, l, succs)
      in
        ((newPreds, newSuccs), (preds', l, succs'))

-- | O(d), where `d` is the degree of the node.
--
-- Remove edges referencing the given node from the given context and return
-- them, as well as the remaining edges. The output structure has the form
-- (preds, succs, altered context), the returned edges are transformed to
-- reference the node they came from.
--
-- >>> matchAndReturnEdges 1 ([("right", 1)], 2, 'b', [("left", 1)])
-- ([("left",2)],[("right",2)],([],2,'b',[]))
--
-- >>> matchAndReturnEdges 3 ([("right", 1)], 2, 'b', [("left", 1)])
-- ([],[],([("right",1)],2,'b',[("left",1)]))
matchAndReturnEdges :: Node -> Context n e -> (Adj e, Adj e, Context n e)
matchAndReturnEdges v (contextPreds, cn, cl, contextSuccs) =
    (newPreds, newSuccs, (remPreds, cn, cl, remSuccs))
  where
    (newSuccs, remPreds) = splitAdjs contextPreds
    (newPreds, remSuccs) = splitAdjs contextSuccs

    -- Use foldl' because we know we need to transfer entire list every time.
    splitAdjs = foldl' splitAdj ([], [])

    splitAdj (new, rest) (el, n) = if n == v
                                 then ((el, cn):new, rest)
                                 else (new, (el, n):rest)

