module Data.Graph where

import           Data.List

-- $setup
-- >>> import qualified Data.Graph.InductiveGraph as G
-- >>> import Data.Char (ord)

-- Here we are using the representation of a graph detaied in Erwig (2001)
-- "Inductive graphs and functional graph algorithms"
-- http://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf
--
-- We can use the directed graph approach to create an undirected graph (as is
-- appropriate to the probem) using undir.
--
-- e = edge label
-- n = node label
--
type Node = Int
type Adj e = [(e, Node)] -- [(edge label, Node to path to)]
type Context n e = (Adj e, Node, n, Adj e)

-- | Labelled node
type LNode n = (Node, n)
-- | Labelled edge
type LEdge e = (Node, Node, e)

-- data Graph a b = Empty | Context a b & Graph a b
class Graph gr where
  -- | Create an empty graph.
  empty :: gr n e

  -- | Is the graph empty?
  isEmpty :: gr n e -> Bool

  -- | Make a graph from lists of labelled nodes and labelled edges.
  mkGraph :: [LNode n] -> [LEdge e] -> gr n e

  -- | Get a list of labelled nodes in the graph.
  labelledNodes :: gr n e -> [LNode n]

  -- | Get a list of labelled edges in the graph.
  labelledEdges :: (Eq e) => gr n e -> [LEdge e]

  -- | Add a context to the given graph.
  (&) :: Context n e -> gr n e -> gr n e

  -- | Same as match, but match any node.
  matchAny :: gr a b -> (Context a b, gr a b)

  -- | Finds the given node in the graph and returns the graph excluding
  -- that node and the context of that node, ready to be reinserted into the
  -- graph.
  --
  -- The (Context & returned Graph) will be isomorphic to the original graph,
  -- although the exact structure may change.
  --
  -- For example, see the Fig. 1 in the paper. It may be constructed as follows:
  --
  -- ([("left", 2), ("up", 3)], 1, 'a', [("right", 2)]) &
  --                       ([], 2, 'b', [("down", 3)])  &
  --                       ([], 3, 'c', [])             & Empty
  --
  -- i.e. 'c' is added first, even though it has predecessors and successors,
  -- those are ignored because at it's time of addition to the graph, no other
  -- nodes exist. The same is true of 'b', although it does have connections to
  -- 'a', only it's connections to 'c' are listed because at the time of it's
  -- addition to the graph, the only other node that exists is 'c'. For this
  -- construction, 'a' is considered to be added to the graph last.
  --
  -- `match 3 graph` will rearrange the graph such that node 3 ('c') is the last
  -- to be added to the graph:
  --
  --  ([("down", 2)], 3, 'c', [("up", 1)])   &
  -- ([("right", 1)], 2, 'b', [("left", 1)]) &
  --             ([], 1, 'a', [])            & Empty
  --
  -- following the same construction logic as before. We say that this graph is
  -- isomorphic to the second.
  match :: Node -> gr a b -> (Maybe (Context a b), gr a b)

infixr 5 &

-- | Returns a list of n nodes that don't already exist in the graph, where n is
-- the given int.
--
-- >>> sort $ newNodes 3 (empty :: G.InductiveGraph String Int)
-- [1,2,3]
-- >>> sort $ newNodes 3 (mkGraph [(1, "A"), (2, "B")] [] :: G.InductiveGraph String Int)
-- [3,4,5]
-- >>> sort $ newNodes 2 (mkGraph [(1, "A"), (3, "B")] [] :: G.InductiveGraph String Int)
-- [4,5]
-- >>> newNodes 0 (empty :: G.InductiveGraph String Int)
-- []
newNodes :: Graph gr => Int -> gr n e -> [Node]
newNodes i g = [n + 1 .. n + i] where n = foldr max 0 (nodes g)

-- | Return a single node that doesn't already exist inside the graph.
--
-- >>> newNode (empty :: G.InductiveGraph String Int)
-- 1
-- >>> newNode (mkGraph [(1, "A"), (2, "B")] [] :: G.InductiveGraph String Int)
-- 3
-- >>> newNode (mkGraph [(1, "A"), (4, "B")] [] :: G.InductiveGraph String Int)
-- 5
newNode :: Graph gr => gr n e -> Node
newNode = (+ 1) . foldr max 0 . nodes

-- | Try to find the node associated with the given label.
--
-- >>> nodeForLabel "A" (mkGraph [(1, "A"), (2, "B")] [] :: G.InductiveGraph String Int)
-- Just 1
-- >>> nodeForLabel "C" (mkGraph [(1, "A"), (2, "B")] [] :: G.InductiveGraph String Int)
-- Nothing
nodeForLabel :: (Graph gr, Eq n) => n -> gr n e -> Maybe Node
nodeForLabel l g = fmap fst $ find (\(_, l') -> l' == l) $ labelledNodes g

-- | Same as nodeForLabel, but add and return the node if it is not already
-- present.
--
-- >>> nodeForLabelMk (mkGraph [(1, "A"), (2, "B")] [] :: G.InductiveGraph String Int) "A"
-- (1,mkGraph [(1,"A"),(2,"B")] [])
-- >>> nodeForLabelMk (mkGraph [(1, "A"), (2, "B")] [] :: G.InductiveGraph String Int) "C"
-- (3,mkGraph [(1,"A"),(2,"B"),(3,"C")] [])
nodeForLabelMk :: (Graph gr, Eq n) => gr n e -> n -> (Node, gr n e)
nodeForLabelMk g l = case nodeForLabel l g of
  Nothing  -> ((n, ([], n, l, []) & g)) where n = newNode g
  (Just n) -> (n, g)


-- | Unordered fold
--
-- >>> ufold (\(_, v, _, _) -> (v +)) 0 (mkGraph [(1, "A"), (2, "B"), (3, "C")] [] :: G.InductiveGraph String Int)
-- 6
-- >>> ufold (\(_, v, _, _) -> (v +)) 0 (mkGraph [] [] :: G.InductiveGraph String Int)
-- 0
ufold :: Graph gr => (Context a b -> c -> c) -> c -> gr a b -> c
ufold f u g | isEmpty g = u
            | otherwise = f c (ufold f u g')
            where (c, g') = matchAny g

-- | Returns a list of nodes in the graph.
--
-- >>> sort $ nodes (mkGraph [(3, "A"), (99, "B")] [] :: G.InductiveGraph String Int)
-- [3,99]
-- >>> nodes (empty :: G.InductiveGraph String Int)
-- []
nodes :: Graph gr => gr a b -> [Node]
nodes = ufold (\(_, v, _, _) -> (v:)) []

-- | Make a directed graph undirected
--
-- >>> sort . labelledEdges $ undir (mkGraph [(1,"A"),(2,"B")] [(1,2,150)] :: G.InductiveGraph String Int)
-- [(1,2,150),(2,1,150)]
-- >>> sort . labelledEdges $ undir (mkGraph [(1,"A"),(2,"B")] [] :: G.InductiveGraph String Int)
-- []
undir :: (Eq b, Graph gr) => gr a b -> gr a b
undir = gmap (\(p, v, l, s) -> let ps = nub (p ++ s) in (ps, v, l, ps))

-- | Map over the nodes in a graph. Preserves the structure of the nodes, but
-- not necessarily the structure of the edges.
--
-- >>> gmap (\(p, v, l, s) -> (p, v, ord l, s)) (mkGraph [(1,'a'),(2,'b')] [(1,2,150)] :: G.InductiveGraph Char Int)
-- mkGraph [(1,97),(2,98)] [(1,2,150)]
gmap :: Graph gr => (Context a b -> Context c d) -> gr a b -> gr c d
gmap f = ufold (\c -> (f c &)) empty

-- | Reverse a graph.
--
-- >>> grev (mkGraph [(1,'a'),(2,'b')] [(1,2,150)] :: G.InductiveGraph Char Int)
-- mkGraph [(1,'a'),(2,'b')] [(2,1,150)]
grev :: Graph gr => gr a b -> gr a b
grev = gmap swap where swap (p, v, l, s) = (s, v, l, p)

-- | Retrieve the successors of a particular node.
--
-- >>> sort $ gsuc 1 (mkGraph [(1,'a'),(2,'b'),(3,'c')] [(1,2,10),(1,3,20)] :: G.InductiveGraph Char Int)
-- [2,3]
-- >>> gsuc 1 (mkGraph [(1,'a'),(2,'b'),(3,'c')] [(2,1,10),(3,1,20)] :: G.InductiveGraph Char Int)
-- []
-- >>> gsuc 1 (mkGraph [(1,'a'),(2,'b'),(3,'c')] [(1,1,10),(1,1,20)] :: G.InductiveGraph Char Int)
-- [1]
gsuc :: Graph gr => Node -> gr a b -> [Node]
gsuc v g = case match v g of
  (Nothing, _) -> []
  (Just c, _)  -> suc c

-- | Retrieve the predecessors of a particular node.
--
-- >>> sort $ gprd 2 (mkGraph [(1,'a'),(2,'b'),(3,'c')] [(1,2,10),(3,2,20)] :: G.InductiveGraph Char Int)
-- [1,3]
-- >>> gprd 2 (mkGraph [(1,'a'),(2,'b'),(3,'c')] [(2,1,10),(3,1,20)] :: G.InductiveGraph Char Int)
-- []
-- >>> gprd 1 (mkGraph [(1,'a'),(2,'b'),(3,'c')] [(1,1,10),(1,1,20)] :: G.InductiveGraph Char Int)
-- [1]
gprd :: Graph gr => Node -> gr a b -> [Node]
gprd v g = case match v g of
  (Nothing, _) -> []
  (Just c, _)  -> prd c

-- | Retrieve the degree of a particular node, loops are counted twice (once as
-- a predecessor, and once as a successor). Return Nothing if node not found in
-- graph.
--
-- >>> deg 1 (mkGraph [(1,'a'),(2,'b')] [(1,2,100)] :: G.InductiveGraph Char Int)
-- Just 1
-- >>> deg 1 (mkGraph [(1,'a'),(2,'b')] [(1,1,100)] :: G.InductiveGraph Char Int)
-- Just 2
-- >>> deg 1 (mkGraph [(1,'a'),(2,'b')] [(1,1,100),(1,1,90)] :: G.InductiveGraph Char Int)
-- Just 4
-- >>> deg 5 (mkGraph [(1,'a'),(2,'b')] [(1,2,100)] :: G.InductiveGraph Char Int)
-- Nothing
deg :: Graph gr => Node -> gr a b -> Maybe Int
deg v g  = case match v g of
  (Nothing, _)           -> Nothing
  (Just (p, _, _, s), _) -> Just (length p + length s)

-- | Remove a node from the graph. Returns original graph if node does not exist
-- in graph. Also removes any connected edges.
--
-- >>> del 1 (mkGraph [(1,'a'),(2,'b')] [(1,2,100)] :: G.InductiveGraph Char Int)
-- mkGraph [(2,'b')] []
-- >>> del 2 (mkGraph [(1,'a'),(2,'b')] [(1,2,100)] :: G.InductiveGraph Char Int)
-- mkGraph [(1,'a')] []
-- >>> del 3 (mkGraph [(1,'a'),(2,'b')] [(1,2,100)] :: G.InductiveGraph Char Int)
-- mkGraph [(1,'a'),(2,'b')] [(1,2,100)]
del :: Graph gr => Node -> gr a b -> gr a b
del v g = case match v g of
  (Nothing, _)  -> g
  (Just _, g')  -> g'

-- | Delete all edges connected to a node. Returns original graph if node does
-- not exist in graph.
--
-- >>> isolate 1 (mkGraph [(1,'a'),(2,'b'),(3,'c')] [(1,1,100),(1,1,90),(2,3,50)] :: G.InductiveGraph Char Int)
-- mkGraph [(1,'a'),(2,'b'),(3,'c')] [(2,3,50)]
-- >>> isolate 5 (mkGraph [(1,'a'),(2,'b'),(3,'c')] [(1,2,90)] :: G.InductiveGraph Char Int)
-- mkGraph [(1,'a'),(2,'b'),(3,'c')] [(1,2,90)]
isolate :: Graph gr => Node -> gr a b -> gr a b
isolate v g = case match v g of
  (Nothing, _) -> g
  (Just (_, _, l, _), g') -> ([], v, l, []) & g'

-- | Insert a list of labelled edges into a graph, for any edge, if one of it's
-- nodes is not present, the edge is not added to the graph.
--
-- sort . labelledEdges $ insEdges [(1,2,50),(2,3,40)] (mkGraph [(1,'a'),(2,'b'),(3,'c')] [] :: G.InductiveGraph Char Int)
-- [(1,2,50), (2,3,40)]
-- insEdges [] (mkGraph [(1,'a'),(2,'b'),(3,'c')] [] :: G.InductiveGraph Char Int)
-- []
insEdges :: Graph gr => [LEdge e] -> gr n e -> gr n e
insEdges es g = foldl' (\g' e -> insEdge e g') g es

-- | Insert a labelled edge into a graph.
--
-- >>> insEdge (1,2,50) (mkGraph [(1,'a'),(2,'b')] [] :: G.InductiveGraph Char Int)
-- mkGraph [(1,'a'),(2,'b')] [(1,2,50)]
-- >>> insEdge (1,1,90) (mkGraph [(1,'a')] [] :: G.InductiveGraph Char Int)
-- mkGraph [(1,'a')] [(1,1,90)]
insEdge :: Graph gr => LEdge e -> gr n e -> gr n e
insEdge (frm, to, e) g = case match frm g of
  (Nothing, g') -> g'
  (Just (ps, n, l, ss), g')  -> case frm == to of
    True  -> ((e,frm):ps, n, l, (e,to):ss) & g'
    False -> (ps, n, l, (e,to):ss) & g'

-- | Update an existing edge between two nodes in the graph, does nothing if
-- either of the two nodes doesn't exist in the graph. Create edge if it doesn't
-- exist.
--
-- >>> updEdge 1 2 70 (mkGraph [(1,"A"),(2,"B")] [(1, 2, 150)] :: G.InductiveGraph String Int)
-- mkGraph [(1,"A"),(2,"B")] [(1,2,70)]
--
-- >>> updEdge 1 2 70 (mkGraph [(1,"A"),(2,"B")] [] :: G.InductiveGraph String Int)
-- mkGraph [(1,"A"),(2,"B")] [(1,2,70)]
--
-- >>> updEdge 1 2 70 (mkGraph [(1,"A"),(2,"B")] [(1, 2, 150), (2, 1, 150)] :: G.InductiveGraph String Int)
-- mkGraph [(1,"A"),(2,"B")] [(2,1,70),(1,2,70)]
--
-- >>> updEdge 3 1 70 (mkGraph [(1,"A"),(2,"B")] [(1, 2, 150)] :: G.InductiveGraph String Int)
-- mkGraph [(1,"A"),(2,"B")] [(1,2,150)]
--
-- >>> updEdge 1 3 70 (mkGraph [(1,"A"),(2,"B")] [(1, 2, 150)] :: G.InductiveGraph String Int)
-- mkGraph [(1,"A"),(2,"B")] [(1,2,150)]
updEdge :: Graph gr => Node -> Node -> b -> gr a b -> gr a b
updEdge s t b' g =
  case (match s g, match t g) of
    -- One of the two nodes doesn't exist
    ((Nothing, _), _) -> g
    (_, (Nothing, _)) -> g
    -- Both nodes exist
    ((Just (ps, n, l, ss), g'), (Just _, _)) -> (ps', n, l, ss') & g'
      where new = (b', t)
            test (_, x) = x == t
            replace p a xs = a : filter (not.p) xs
            (ps', ss') = case (find test ps, find test ss) of
              (Nothing, Nothing)   -> (ps, new:ss)
              (Nothing, (Just _))  -> (ps, replace test new ss)
              ((Just _), Nothing)  -> (replace test new ps, ss)
              ((Just _), (Just _)) -> (replace test new ps, replace test new ss)

-- | Retrieve the successors from a known context.
--
-- >>> suc ([("edge1", 2)], 1, "A", [("edge2", 3)])
-- [3]
-- >>> sort $ suc ([], 1, "A", [("edge1", 2), ("edge2", 3)])
-- [2,3]
-- >>> suc ([], 1, "A", [])
-- []
suc :: Context a b -> [Node]
suc (_, _, _, s) = nub . map snd $ s

-- | Retrieve the predecessors from a known context.
--
-- >>> prd ([("edge1", 2)], 1, "A", [("edge2", 3)])
-- [2]
-- >>> sort $ prd ([("edge1", 2), ("edge2", 3)], 1, "A", [])
-- [2,3]
-- >>> prd ([], 1, "A", [])
-- []
prd :: Context a b -> [Node]
prd (p, _, _, _) = nub . map snd $ p
