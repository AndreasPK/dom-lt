{-# LANGUAGE BangPatterns #-}

-- | Loads the GHC CFG samples
--
-- The file is gzip-compressed:
-- * Three groups of graphs, separated by a "-" line
-- * One graph per line: n_samples n_nodes a1 b1 a2 b2 ...
--   + @n_samples@: How many graphs of this shape were in the original dataset.
--   + @n_nodes@: Take a guess
--   + @ai bi@: Directed edges.
--
-- I've pruned the dataset to only keep every 8th of the larger graphs.
module Loader (Group, loadGraphs) where

import Data.Graph.Dom (Rooted, fromEdges)

import qualified Codec.Compression.GZip as GZ
import Control.Monad (foldM)
import Data.Array (Array, listArray)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import GHC.Compact (Compact, compact, compactAdd, getCompact, compactAddWithSharing)
import System.IO.Error (catchIOError)
import System.Exit (exitFailure)

-- (CFGs of these shapes in the data, actual graphs)
type Group = (Int, Array Int Rooted)
emptyGroup = (0, listArray (0,0) [])

-- | Load the three groups: smallest 80%, 80%-95%, largest 5% of the CFGs.
--
--  We store them in a compact region so they don't mess with benchmark performance.
loadGraphs :: FilePath -> IO (Group, Group, Group)
loadGraphs path = do
  flip catchIOError (\err -> do
        putStrLn $ "WARNING: Couldn't load GHC CFG Samples at " ++ show path
        exitFailure
      ) $ do
        input <- GZ.decompress <$> BL.readFile path
        region <- compact ()
        groups <- splitGroups region (BL.lines input)
        case groups of
          [g1, g2, g3] -> return (g1, g2, g3)
          _ -> fail $ path ++ ": expected 3 groups separated by '-', found "
                          ++ show (length groups)

-- | Parse all graphs, add them to the compact region and return the groups.
splitGroups :: Compact () -> [BL.ByteString] -> IO [Group]
splitGroups _ [] = return []
splitGroups region ls = do
  let (groupLines, rest) = break (== BL.pack "-") ls
  g <- toGroup groupLines
  (g :) <$> splitGroups region (drop 1 rest)
  where
    toGroup groupLines = do
      -- One pass: each parsed graph is garbage once it is in the region.
      (cfg_count, n, rev_graphs) <- foldM addGraph (0, 0, []) groupLines
      -- yay for compact regions
      arr <- getCompact <$> compactAddWithSharing region (listArray (0, n - 1) (reverse rev_graphs))
      return (cfg_count, arr)

    addGraph (!count, !n, gs) l = do
      let (c, g) = parseLine l
      g' <- getCompact <$> compactAdd region g
      return (count + c, n + 1, g' : gs)

-- | Parse @count n a1 b1 ...@. The graph has a key for every node @1..n@,
-- also for the ones without successors.
parseLine :: BL.ByteString -> (Int, Rooted)
parseLine l = case map int (BL.words l) of
  !count : !n : es -> (count, (1, fromEdges (pairs es) `IM.union` noSuccs n))
  _              -> bad
  where
    int w = maybe bad fst (BL.readInt w)
    -- Graph of nodes without edges
    noSuccs n = IM.fromList [(v, IS.empty) | v <- [1..n]]
    pairs (a:b:rest) = (a, b) : pairs rest
    pairs []         = []
    pairs _          = bad
    bad = error ("Loader: bad line: " ++ BL.unpack l)
