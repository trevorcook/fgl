-----------------------------------------------------------------------------
--
-- Module      :  Data.Graph.Inductive.Generic
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Data.Graph.Inductive.Generic (
    module Data.Graph.Inductive.Generic.Graph,
    module Data.Graph.Inductive.Generic.Tree,
    module Data.Graph.Inductive.Generic.Basic,
    module Data.Graph.Inductive.Generic.Monad,
    module Data.Graph.Inductive.Monad.IOArray,
    module Data.Graph.Inductive.Generic.Query,
--    module Data.Graph.Inductive.NodeMap,
    -- * Version Information
    version
) where

import Data.Graph.Inductive.Generic.Graph
import Data.Graph.Inductive.Generic.Tree
import Data.Graph.Inductive.Generic.Basic
import Data.Graph.Inductive.Generic.Monad
import Data.Graph.Inductive.Monad.IOArray
--import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.Generic.Query

import           Data.Version (showVersion)
import qualified Paths_fgl    as Paths (version)

-- | Version info
version :: IO ()
version = putStrLn $ "\nFGL - Functional Graph Library, version "
                      ++ showVersion Paths.version

