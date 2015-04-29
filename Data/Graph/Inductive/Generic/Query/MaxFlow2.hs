-- | Alternative Maximum Flow
module Data.Graph.Inductive.Generic.Query.MaxFlow2(
    Network,
    ekSimple, ekFused, ekList,
) where

--   ekSimple, ekFused, ekList) where


import Data.Maybe

import Data.Graph.Inductive.Generic.Graph
import Data.Graph.Inductive.Generic.Tree
import Data.Graph.Inductive.Generic.Query.BFS      (bft)

import Data.Graph.Inductive.Internal.Queue

import           Data.Set (Set)
import qualified Data.Set as S

------------------------------------------------------------------------------
-- Data types

-- Network data type
type Network n = Gr n () (Double, Double)

-- Data type for direction in which an edge is traversed
data Direction = Forward | Backward
    deriving (Eq, Ord, Show, Read)

-- Data type for edge with direction of traversal
type DirEdge n b = (n, n, b, Direction)

type DirPath n =[(n, Direction)]
type DirRTree n =[DirPath n]

pathFromDirPath :: DirPath n-> [n]
pathFromDirPath = map fst

------------------------------------------------------------------------------
-- Example networks

-- Example number 1
-- This network has a maximum flow of 2000
{-
exampleNetwork1 :: Network
exampleNetwork1=mkGraph [ (1,()), (2,()), (3,()), (4,()) ]
    [ (1,2,(1000,0)), (1,3,(1000,0)),
    (2,3,(1,0)), (2,4,(1000,0)), (3,4,(1000,0)) ]

-- Example number 2
-- Taken from "Introduction to Algorithms" (Cormen, Leiserson, Rivest)
-- This network has a maximum flow of 23
exampleNetwork2 :: Network
-- Names of nodes in "Introduction to Algorithms":
-- 1: s
-- 2: v1
-- 3: v2
-- 4: v3
-- 5: v4
-- 6: t
exampleNetwork2=mkGraph [ (1,()), (2,()), (3,()), (4,()), (5,()), (6,()) ]
    [ (1, 2, (16, 0)),
    (1, 3, (13, 0)),
    (2, 3, (10, 0)),
    (3, 2, (4, 0)),
    (2, 4, (12, 0)),
    (3, 5, (14, 0)),
    (4, 3, (9, 0)),
    (5, 4, (7, 0)),
    (4, 6, (20, 0)),
    (5, 6, (4, 0)) ]
-}
------------------------------------------------------------------------------
-- Implementation of Edmonds-Karp algorithm

-- EXTRACT fglEdmondsFused.txt
-- Compute an augmenting path
augPathFused :: Ord n => Network n -> n -> n -> Maybe (DirPath n)
augPathFused g s t = listToMaybe $ map reverse $
    filter (\((u,_):_) -> u==t) tree
    where tree = bftForEK s g

-- Breadth First Search wrapper function
bftForEK :: Ord n => n -> Network n -> DirRTree n
bftForEK v = bfForEK (queuePut [(v,Forward)] mkQueue)

-- Breadth First Search, tailored for Edmonds & Karp
bfForEK :: Ord n => Queue (DirPath n) -> Network n -> DirRTree n
bfForEK q g
    | queueEmpty q || isEmpty g = []
    | otherwise                 = case match v g of
        (Nothing, g')                     -> bfForEK q1 g'
        (Just (preAdj, _, _, sucAdj), g') -> p:bfForEK q2 g'
            where
                -- Insert successor nodes (with path to root) into queue
                q2   = queuePutList suc1 $ queuePutList suc2 q1
                -- Traverse edges in reverse if flow positive
                suc1 = [ (preNode, Backward):p
                    | ((_, f), preNode) <- preAdj, f>0]
                -- Traverse edges forwards if flow less than capacity
                suc2 = [ (sucNode,Forward):p
                    | ((c, f), sucNode) <- sucAdj, c>f]
    where (p@((v,_):_), q1)=queueGet q

-- Extract augmenting path from network; return path as a sequence of
-- edges with direction of traversal, and new network with augmenting
-- path removed.
extractPathFused :: Ord n => Network n -> DirPath n
    -> ([DirEdge n (Double,Double)], Network n)
extractPathFused g []  = ([], g)
extractPathFused g [(_,_)] = ([], g)
extractPathFused g ((u,_):rest@((v,Forward):_)) =
    ((u, v, l, Forward):tailedges, newerg)
        where (tailedges, newerg) = extractPathFused newg rest
              Just (l, newg)    = extractEdge g u v (\(c,f)->(c>f))
extractPathFused g ((u,_):rest@((v,Backward):_)) =
    ((v, u, l, Backward):tailedges, newerg)
        where (tailedges, newerg) = extractPathFused newg rest
              Just (l, newg)    = extractEdge g v u (\(_,f)->(f>0))

ekFusedStep :: Ord n => EKStepFunc n
ekFusedStep g s t = case maybePath of
        Just _          ->
            Just ((insEdges (integrateDelta es delta) newg), delta)
        Nothing   -> Nothing
    where maybePath     = augPathFused g s t
          (es, newg) = extractPathFused g (fromJust maybePath)
          delta         = minimum $ getPathDeltas es

ekFused :: Ord n => Network n-> n -> n -> (Network n, Double)
ekFused = ekWith ekFusedStep
-- ENDEXTRACT

-----------------------------------------------------------------------------
-- Alternative implementation: Use an explicit residual graph

-- EXTRACT fglEdmondsSimple.txt
residualGraph :: Ord n => Network n -> Gr n () Double
residualGraph g =
    mkGraph (labNodes g)
        ([(u, v, c-f) | (u, v, (c,f)) <- labEdges g, c>f ] ++
         [(v, u, f) | (u,v,(_,f)) <- labEdges g, f>0])

augPath :: Ord n => Network n -> n -> n -> Maybe (Path n)
augPath g s t = listToMaybe $ map reverse $ filter (\(u:_) -> u==t) tree
    where tree = bft s (residualGraph g)

-- Extract augmenting path from network; return path as a sequence of
-- edges with direction of traversal, and new network with augmenting
-- path removed.
extractPath :: Ord n => Network n -> Path n -> ([DirEdge n (Double,Double)], Network n)
extractPath g []  = ([], g)
extractPath g [_] = ([], g)
extractPath g (u:v:ws) =
    case fwdExtract of
        Just (l, newg) -> ((u, v, l, Forward):tailedges, newerg)
            where (tailedges, newerg) = extractPath newg (v:ws)
        Nothing          ->
            case revExtract of
                Just (l, newg) ->
                    ((v, u, l, Backward):tailedges, newerg)
                    where (tailedges, newerg) = extractPath newg (v:ws)
                Nothing               -> error "extractPath: revExtract == Nothing"
    where fwdExtract = extractEdge g u v (\(c,f)->(c>f))
          revExtract = extractEdge g v u (\(_,f)->(f>0))

-- Extract an edge from the graph that satisfies a given predicate
-- Return the label on the edge and the graph without the edge
extractEdge :: Ord n => Gr n a b -> n -> n -> (b->Bool) -> Maybe (b, Gr n a b)
extractEdge g u v p =
    case adj of
        Just (el, _) -> Just (el, (p', node, l, rest) & newg)
        Nothing      -> Nothing
    where (Just (p', node, l, s), newg) = match u g
          (adj, rest)=extractAdj s
              (\(l', dest) -> (dest==v) && (p l'))

-- Extract an item from an adjacency list that satisfies a given
-- predicate. Return the item and the rest of the adjacency list
extractAdj :: Adj n b -> ((b,n)->Bool) -> (Maybe (b,n), Adj n b)
extractAdj []         _ = (Nothing, [])
extractAdj (adj:adjs) p
    | p adj     = (Just adj, adjs)
    | otherwise = (theone, adj:rest)
        where (theone, rest)=extractAdj adjs p

getPathDeltas :: [DirEdge n (Double,Double)] -> [Double]
getPathDeltas []     = []
getPathDeltas (e:es) = case e of
    (_, _, (c,f), Forward)  -> (c-f) : (getPathDeltas es)
    (_, _, (_,f), Backward) -> f : (getPathDeltas es)

integrateDelta :: [DirEdge n (Double,Double)] -> Double
    -> [LEdge n (Double, Double)]
integrateDelta []          _ = []
integrateDelta (e:es) delta = case e of
    (u, v, (c, f), Forward) ->
        (u, v, (c, f+delta)) : (integrateDelta es delta)
    (u, v, (c, f), Backward) ->
        (u, v, (c, f-delta)) : (integrateDelta es delta)

type EKStepFunc n = Network n -> n -> n -> Maybe (Network n, Double)

ekSimpleStep :: Ord n => EKStepFunc n
ekSimpleStep g s t = case maybePath of
        Just _ ->
            Just ((insEdges (integrateDelta es delta) newg), delta)
        Nothing   -> Nothing
    where maybePath  = augPath g s t
          (es, newg) = extractPath g (fromJust maybePath)
          delta      = minimum $ getPathDeltas es

ekWith :: EKStepFunc n -> Network n -> n -> n -> (Network n, Double)
ekWith stepfunc g s t = case stepfunc g s t of
    Just (newg, delta) -> (finalg, capacity+delta)
        where (finalg, capacity) = (ekWith stepfunc newg s t)
    Nothing            -> (g, 0)

ekSimple :: Ord n => Network n -> n -> n -> (Network n, Double)
ekSimple = ekWith ekSimpleStep
-- ENDEXTRACT

-----------------------------------------------------------------------------
-- Alternative implementation: Process list of edges to extract path instead
-- of operating on graph structure

extractPathList :: Ord n => [LEdge n (Double, Double)] -> Set (n,n)
    -> ([DirEdge n (Double, Double)], [LEdge n (Double, Double)])
extractPathList []                 _ = ([], [])
extractPathList (edge@(u,v,l@(c,f)):es) set
    | (c>f) && (S.member (u,v) set) =
        let (pathrest, notrest)=extractPathList es (S.delete (u,v) set)
            in ((u,v,l,Forward):pathrest, notrest)
    | (f>0) && (S.member (v,u) set) =
        let (pathrest, notrest)=extractPathList es (S.delete (u,v) set)
            in ((u,v,l,Backward):pathrest, notrest)
    | otherwise                        =
        let (pathrest, notrest)=extractPathList es set in
            (pathrest, edge:notrest)

ekStepList :: Ord n => EKStepFunc n
ekStepList g s t = case maybePath of
        Just _  -> Just (mkGraph (labNodes g) newEdges, delta)
        Nothing -> Nothing
    where newEdges      = (integrateDelta es delta) ++ otheredges
          maybePath     = augPathFused g s t
          (es, otheredges) = extractPathList (labEdges g)
              (S.fromList (zip justPath (tail justPath)))
          delta         = minimum $ getPathDeltas es
          justPath      = pathFromDirPath (fromJust maybePath)

ekList :: Ord n => Network n -> n -> n -> (Network n, Double)
ekList = ekWith ekStepList
-- ENDEXTRACT
