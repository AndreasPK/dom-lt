module Data.Graph.Dom.Internal where

import Data.Graph.Dom as D

import Data.Foldable as F
import Data.List
import Data.IntMap.Strict as IM
import Data.IntSet as IS

newline :: [Char]
newline = "\n"

-- | For debugging only
asDotFile :: D.Graph -> String
asDotFile g =
    let pprNode :: (Int, IntSet) -> String
        pprNode (node,targets) =
            concat $ intersperse newline $ fmap (pprEdge node) $ IS.toList targets
        pprEdge v u = show v ++ " -> " ++ show u ++ ";"
    in "digraph G {" ++ newline ++
            (concat $ intersperse newline $ fmap pprNode (IM.toList g)) ++ newline ++
            "}"