-- (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
-- | Minimum-Spanning-Tree Algorithms

module Data.Graph.Inductive.Generic.Query.MST (
    msTreeAt,msTree,
    -- * Path in MST
    msPath
) where

import           Data.Graph.Inductive.Generic.Graph
import qualified Data.Graph.Inductive.Internal.Heap     as H
import           Data.Graph.Inductive.Generic.Internal.RootPath


newEdges :: (Ord b) => LPath n b -> Context n a b -> [H.Heap b (LPath n b)]
newEdges (LP p) (_,_,_,s) = map (\(l,v)->H.unit l (LP ((v,l):p))) s

prim :: (Graph gr,Real b) => H.Heap b (LPath (Node gr) b) -> gr a b -> LRTree (Node gr) b
prim h g | H.isEmpty h || isEmpty g = []
prim h g =
    case match v g of
         (Just c,g')  -> p:prim (H.mergeAll (h':newEdges p c)) g'
         (Nothing,g') -> prim h' g'
    where (_,p@(LP ((v,_):_)),h') = H.splitMin h

msTreeAt :: (Graph gr,Real b) => Node gr -> gr a b -> LRTree (Node gr) b
msTreeAt v g = prim (H.unit 0 (LP [(v,0)])) g

msTree :: (Graph gr,Real b) => gr a b -> LRTree (Node gr) b
msTree g = msTreeAt v g where ((_,v,_,_),_) = matchAny g

msPath :: (Real b, Eq n) => LRTree n b -> n -> n -> Path n
msPath t a b = joinPaths (getLPathNodes a t) (getLPathNodes b t)

joinPaths :: Eq n => Path n -> Path n -> Path n
joinPaths p q = joinAt (head p) p q

joinAt :: Eq n => n -> Path n -> Path n -> Path n
joinAt _ (v:vs) (w:ws) | v==w = joinAt v vs ws
joinAt x p      q             = reverse p++(x:q)
