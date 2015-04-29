-- (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
-- | Inward directed trees as lists of paths.
module Data.Graph.Inductive.Generic.Internal.RootPath (
    -- * Types
    RTree,LRTree,
    -- * Operations
    getPath,getLPath,
    getDistance,
    getLPathNodes
) where

import Data.Graph.Inductive.Generic.Graph

type LRTree n a = [LPath n a]
type RTree n = [Path n]

first :: ([a] -> Bool) -> [[a]] -> [a]
first p xss  = case filter p xss of
                 []   -> []
                 x:_  -> x

-- | Find the first path in a tree that starts with the given node
findP :: Eq n => n -> LRTree n a -> [LNode n a]
findP _ []                                  = []
findP v ((LP []):ps)                        = findP v ps
findP v ((LP (p@((w,_):_))):ps) | v==w      = p
                                | otherwise = findP v ps

getPath :: Eq n => n -> RTree n -> Path n
getPath v = reverse . first (\(w:_)->w==v)

getLPath :: Eq n => n -> LRTree n a -> LPath n a
getLPath v = LP . reverse . findP v

getDistance :: Eq n => n -> LRTree n a -> a
getDistance v = snd . head . findP v

getLPathNodes :: Eq n => n -> LRTree n a -> Path n
getLPathNodes v = (\(LP p)->map fst p) . getLPath v
