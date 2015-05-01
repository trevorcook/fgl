{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

-- (c) 1999 - 2002 by Martin Erwig [see file COPYRIGHT]
-- | Tree-based implementation of 'Graph' and 'DynGraph'
--
--   You will probably have better performance using the
--   "Data.Graph.Inductive.PatriciaTree" implementation instead.

module Data.Graph.Inductive.Generic.Tree (Gr,UGr) where

import Data.Graph.Inductive.Generic.Graph

import           Control.Applicative (liftA2)
import           Control.Arrow       (first, second)
import           Control.DeepSeq     (NFData (..))
import           Data.List           (foldl', sort)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics        (Generic)
#endif

----------------------------------------------------------------------
-- GRAPH REPRESENTATION
----------------------------------------------------------------------

newtype Gr n a b = Gr (GraphRep n a b)
#if __GLASGOW_HASKELL__ >= 702
  deriving (Generic)
#endif

type GraphRep n a b = Map n (Context' n a b)
type Context' n a b = (Adj n b,a,Adj n b)

type UGr n = Gr n () ()

----------------------------------------------------------------------
-- CLASS INSTANCES
----------------------------------------------------------------------

instance (Eq a, Ord b, Ord n) => Eq (Gr n a b) where
  (Gr g1) == (Gr g2) = fmap sortAdj g1 == fmap sortAdj g2
    where
      sortAdj (p,n,s) = (sort p,n,sort s)

instance (Show a, Show b, Show n, Ord n) => Show (Gr n a b) where
  showsPrec d g = showParen (d > 10) $
                    showString "mkGraph "
                    . shows (labNodes g)
                    . showString " "
                    . shows (labEdges g)

instance (Read a, Read b, Read n, Ord n) => Read (Gr n a b) where
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("mkGraph", s) <- lex r
    (ns,t) <- reads s
    (es,u) <- reads t
    return (mkGraph ns es, u)

-- Graph
--
instance (Ord n) => Graph (Gr n) where
  type Node (Gr n) = n
  empty             = Gr M.empty

  isEmpty (Gr g)    = M.null g

  match v gr@(Gr g) = maybe (Nothing, gr)
                            (first Just . uncurry (cleanSplit v))
                      . (\(m,g') -> fmap (flip (,) g') m)
                      $ M.updateLookupWithKey (const (const Nothing)) v g

  mkGraph vs es     = insEdges es
                      . Gr
                      . M.fromList
                      . map (second (\l -> ([],l,[])))
                      $ vs

  labNodes (Gr g)   = map (\(v,(_,l,_))->(v,l)) (M.toList g)

  matchAny (Gr g)   = maybe (error "Match Exception, Empty Graph")
                            (uncurry (uncurry cleanSplit))
                            (M.minViewWithKey g)

  noNodes   (Gr g)  = M.size g

  nodeRange (Gr g)  = fromMaybe (error "nodeRange of empty graph")
                      $ liftA2 (,) (ix (M.minViewWithKey g))
                                   (ix (M.maxViewWithKey g))
    where
      ix            = fmap (fst . fst)

  labEdges  (Gr g)  = concatMap (\(v,(_,_,s))->map (\(l,w)->(v,w,l)) s) (M.toList g)

-- After a Node (with its corresponding Context') are split out of a
-- GraphRep, clean up the remainders.
cleanSplit :: Ord n => n -> Context' n a b -> GraphRep n a b
              -> (Context n a b, Gr n a b)
cleanSplit v (p,l,s) g = (c, Gr g')
  where
    -- Note: loops are kept only in successor list
    c = (p', v, l, s)
    p' = rmLoops p
    s' = rmLoops s
    rmLoops = filter ((/=v) . snd)

    g' = updAdj s' (clearPred v) . updAdj p' (clearSucc v) $ g

-- DynGraph
--
instance Ord n => DynGraph (Gr n) where
  (p,v,l,s) & (Gr g) = Gr
                       . updAdj p (addSucc v)
                       . updAdj s (addPred v)
                       $ M.insert v (p,l,s) g
--    where
--      addCntxt = maybe onAbsent onPresent
--      onAbsent = Just cntxt'
--      --Originally
--      onPresent = const (error ("Node Exception: Node already exists"))
----      onPresent  = Just cntxt'
--      cntxt' = (p,l,s)

--instance Ord n => DynGraph (Gr n) where
--    (p, v, l, s) & (Gr g)
--        = let !g1 = M.insert v (fromAdj p, l, fromAdj s) g
--              !g2 = addSucc g1 v p
--              !g3 = addPred g2 v s
--          in Gr g3


instance (NFData a, NFData b, NFData n) => NFData (Gr n a b) where
  rnf (Gr g) = rnf g

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

toAdj :: Ord n => Map n [b] -> Adj n b
toAdj = concatMap expand . M.toList
  where
    expand (n,ls) = map (flip (,) n) ls

fromAdj :: Ord n => Adj n b -> Map n [b]
fromAdj = M.fromListWith addLists . map (second (:[]) . swap)

addSucc :: n -> b -> Context' n a b -> Context' n a b
addSucc v l (p,l',s) = (p,l',(l,v):s)

addPred :: n -> b -> Context' n a b -> Context' n a b
addPred v l (p,l',s) = ((l,v):p,l',s)

clearSucc :: Eq n => n -> b -> Context' n a b -> Context' n a b
clearSucc v _ (p,l,s) = (p,l,filter ((/=v).snd) s)

clearPred :: Eq n => n-> b -> Context' n a b -> Context' n a b
clearPred v _ (p,l,s) = (filter ((/=v).snd) p,l,s)

updAdj :: Ord n => Adj n b -> (b -> Context' n a b -> Context' n a b)
       -> GraphRep n a b -> GraphRep n a b
updAdj adj f g = foldl' (\g' (l,v) -> M.adjust (f l) v g') g adj

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

addLists :: [a] -> [a] -> [a]
addLists [a] as  = a : as
addLists as  [a] = a : as
addLists xs  ys  = xs ++ ys
