-- (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]

-- | Shortest path algorithms
module Data.Graph.Inductive.Generic.Query.SP(
    spTree,spLength,sp,
    dijkstra
) where

import qualified Data.Graph.Inductive.Internal.Heap as H

import Data.Graph.Inductive.Generic.Graph
import Data.Graph.Inductive.Generic.Internal.RootPath

expand :: (Real b) => b -> LPath n b -> Context n a b -> [H.Heap b (LPath n b)]
expand d (LP p) (_,_,_,s) = map (\(l,v)->H.unit (l+d) (LP ((v,l+d):p))) s

-- | Implementation of Dijkstra's shortest path algorithm
dijkstra :: (Graph gr, Real b)
         => H.Heap b (LPath (Node gr) b) -> gr a b -> LRTree (Node gr) b
dijkstra h g | H.isEmpty h || isEmpty g = []
dijkstra h g =
    case match v g of
         (Just c,g')  -> p:dijkstra (H.mergeAll (h':expand d p c)) g'
         (Nothing,g') -> dijkstra h' g'
    where (_,p@(LP ((v,d):_)),h') = H.splitMin h

spTree :: (Graph gr, Real b) => Node gr -> gr a b -> LRTree (Node gr) b
spTree v = dijkstra (H.unit 0 (LP [(v,0)]))

spLength :: (Graph gr, Real b) => Node gr -> Node gr -> gr a b -> b
spLength s t = getDistance t . spTree s

sp :: (Graph gr, Real b) => Node gr -> Node gr -> gr a b -> Path (Node gr)
sp s t = getLPathNodes t . spTree s
