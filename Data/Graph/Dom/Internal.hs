{-# LANGUAGE CPP #-}

module Data.Graph.Dom.Internal where

import Data.Graph.Dom as D

#if __GLASGOW_HASKELL__ >= 800
import Data.IntMap.Strict as IM
#else
import Data.IntMap as IM
#endif

import Data.Foldable as F
import Data.List
import Data.IntSet as IS

newline :: [Char]
newline = "\n"

-- | For debugging only
asDotFile :: D.Graph -> String
asDotFile g =
    let pprNode :: (Int, IntSet) -> String
        pprNode (node,targets) =
            F.concat $ intersperse newline $ fmap (pprEdge node) $ IS.toList targets
        pprEdge v u = show v ++ " -> " ++ show u ++ ";"
    in "digraph G {" ++ newline ++
            (F.concat $ intersperse newline $ fmap pprNode (IM.toList g)) ++ newline ++
            "}"