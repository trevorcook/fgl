-- (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
-- | Graph Voronoi Diagram

module Data.Graph.Inductive.Generic.Query.GVD (
    Voronoi,
    gvdIn,gvdOut,
    voronoiSet,nearestNode,nearestDist,nearestPath,
--    vd,nn,ns,
--    vdO,nnO,nsO
) where

import Data.List  (nub)
import Data.Maybe (listToMaybe)

import qualified Data.Graph.Inductive.Internal.Heap as H

import Data.Graph.Inductive.Generic.Basic
import Data.Graph.Inductive.Generic.Graph
import Data.Graph.Inductive.Generic.Internal.RootPath
import Data.Graph.Inductive.Generic.Query.SP          (dijkstra)

type Voronoi n a = LRTree n a

gvdIn :: (DynGraph gr, Real b) => [Node gr] -> gr a b -> Voronoi (Node gr) b
gvdIn vs g = gvdOut vs (grev g)

gvdOut :: (Graph gr, Real b) => [Node gr] -> gr a b -> Voronoi (Node gr) b
gvdOut vs = dijkstra (H.build (zip (repeat 0) (map (\v->LP [(v,0)]) vs)))

voronoiSet :: (Real b,Eq n) => n -> Voronoi n b -> [n]
voronoiSet v = nub . concat . filter (\p->last p==v) . map (\(LP p)->map fst p)

maybePath :: (Real b,Eq n) => n -> Voronoi n b -> Maybe (LPath n b)
maybePath v = listToMaybe . filter (\(LP ((w,_):_))->w==v)

nearestNode :: (Real b,Eq n) => n -> Voronoi n b -> Maybe n
nearestNode v = fmap (\(LP ((w,_):_))->w) . maybePath v

nearestDist :: (Real b,Eq n) => n -> Voronoi n b -> Maybe b
nearestDist v = fmap (\(LP ((_,l):_))->l) . maybePath v

nearestPath :: (Real b,Eq n) => n -> Voronoi n b -> Maybe (Path n)
nearestPath v = fmap (\(LP p)->map fst p) . maybePath v


-- vd = gvdIn [4,5] vor
-- vdO = gvdOut [4,5] vor
-- nn = map (flip nearestNode vd) [1..8]
-- nnO = map (flip nearestNode vdO) [1..8]
-- ns = map (flip voronoiSet vd) [1..8]
-- nsO = map (flip voronoiSet vdO) [1..8]