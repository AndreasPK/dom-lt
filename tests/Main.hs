{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-signatures #-}
#endif

module Main (main) where

import Data.Graph.Dom as G
import Data.Tree
import System.Exit

import Test.HUnit hiding (Node,Path)

g0 :: Graph
g0 = fromAdj
  [(1,[2,3])
  ,(2,[3])
  ,(3,[4])
  ,(4,[3,5,6])
  ,(5,[7])
  ,(6,[7])
  ,(7,[4,8])
  ,(8,[3,9,10])
  ,(9,[1])
  ,(10,[7])]

g1 :: Graph
g1 = fromAdj
  [(0,[1])
  ,(1,[2,3])
  ,(2,[7])
  ,(3,[4])
  ,(4,[5,6])
  ,(5,[7])
  ,(6,[4])
  ,(7,[])]

main = do
  result_counts <- runTestTT tests
  putStrLn $ showCounts result_counts
  if (errors result_counts + failures result_counts) > 0
    then exitWith $ ExitFailure 1
    else exitWith   ExitSuccess

tests :: Test
tests =
  TestList $
    map TestCase
      [dom0, dom1, pdom0, pdom1, ipdom0, ipdom1, dom_t_g0, dom_t_g1, dom_pt_g0, dom_pt_g1,
       dom_T2_g3]

g0_rooted, g1_rooted :: Rooted
g0_rooted = (1,g0)
g1_rooted = (0,g1)

dom0 = assertEqual "dom g0"
        [(2,[1]),(3,[1]),(4,[3,1]),(5,[4,3,1]),(6,[4,3,1]),(7,[4,3,1]),(8,[7,4,3,1]),(9,[8,7,4,3,1]),(10,[8,7,4,3,1])]
        (dom g0_rooted)

dom1 = assertEqual "dom g1"
        [(1,[0]),(2,[1,0]),(3,[1,0]),(7,[1,0]),(4,[3,1,0]),(5,[4,3,1,0]),(6,[4,3,1,0])]
        (dom g1_rooted)

pdom0 = assertEqual "pdom g0"
        [(9,[1]),(8,[9,1]),(7,[8,9,1]),(4,[7,8,9,1]),(5,[7,8,9,1]),(6,[7,8,9,1]),(10,[7,8,9,1]),(3,[4,7,8,9,1]),(2,[3,4,7,8,9,1])]
        (pdom g0_rooted)

pdom1 = assertEqual "pdom g1"
        []
        (pdom g1_rooted)

ipdom0 = assertEqual "ipdom g0"
          [(10,7),(8,9),(9,1),(7,8),(6,7),(5,7),(4,7),(3,4),(2,3),(1,1)]
          (ipdom g0_rooted)

ipdom1 = assertEqual "ipdom g1"
          [(0,0)]
          (ipdom g1_rooted)

dom_t_g0 = assertEqual "domTree g0"
            (  Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []},Node {rootLabel = 7, subForest = [Node {rootLabel = 8, subForest = [Node {rootLabel = 9, subForest = []},Node {rootLabel = 10, subForest = []}]}]}]}]}]}  )
            (domTree g0_rooted)

dom_t_g1 = assertEqual "domTree g1"
            (  Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []}]}]}, Node {rootLabel = 7, subForest = []}]}]}  )
            (domTree g1_rooted)

dom_pt_g0 = assertEqual "pdomTree g0"
              (  Node {rootLabel = 1, subForest = [Node {rootLabel = 9, subForest = [Node {rootLabel = 8, subForest = [Node {rootLabel = 7, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 3, subForest = [Node {rootLabel = 2, subForest = []}]}]},Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []},Node {rootLabel = 10, subForest = []}]}]}]}]}  )
              (pdomTree g0_rooted)

dom_pt_g1 = assertEqual "pdomTree g1"
              (  Node {rootLabel = 0, subForest = []}  )
              (pdomTree g1_rooted)

g3_edges :: Graph
g3_edges = fromAdj [(0,[3,6]), (3,[8,9]), (6,[99]), (8,[11]), (9,[16]), (11,[99]), (12,[99]), (13,[99]), (16,[12,13]), (99,[])]
g3 :: (Int, Graph)
g3 = (0 :: Int, g3_edges)

dom_T2_g3 :: Assertion
dom_T2_g3 = assertEqual "dom #2"
              [(99,[0])]
              ( filter (\x -> fst x == 99) $ dom g3)


-- applyDomFunctions :: Rooted
--                   -> ([(Node, Path)], [(Node, Path)], [(Node, Node)], [(Node, Node)], Tree Node, Tree Node)
-- applyDomFunctions g = (dom g, pdom g, idom g, ipdom g, domTree g, pdomTree g)

-- g0_expected :: ([(Node, Path)], [(Node, Path)], [(Node, Node)], [(Node, Node)], Tree Node, Tree Node)
-- g0_expected = (
--   [(2,[1]),(3,[1]),(4,[3,1]),(5,[4,3,1]),(6,[4,3,1]),(7,[4,3,1]),(8,[7,4,3,1]),(9,[8,7,4,3,1]),(10,[8,7,4,3,1])],
--   [(9,[1]),(8,[9,1]),(7,[8,9,1]),(4,[7,8,9,1]),(5,[7,8,9,1]),(6,[7,8,9,1]),(10,[7,8,9,1]),(3,[4,7,8,9,1]),(2,[3,4,7,8,9,1])],
--   [(10,8),(7,4),(9,8),(1,1),(8,7),(3,1),(4,3),(6,4),(5,4),(2,1)],
--   [(10,7),(8,9),(9,1),(7,8),(6,7),(5,7),(4,7),(3,4),(2,3),(1,1)],
  -- Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []},Node {rootLabel = 7, subForest = [Node {rootLabel = 8, subForest = [Node {rootLabel = 9, subForest = []},Node {rootLabel = 10, subForest = []}]}]}]}]}]},
  -- Node {rootLabel = 1, subForest = [Node {rootLabel = 9, subForest = [Node {rootLabel = 8, subForest = [Node {rootLabel = 7, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 3, subForest = [Node {rootLabel = 2, subForest = []}]}]},Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []},Node {rootLabel = 10, subForest = []}]}]}]}]}
--   )

-- g1_expected :: ([(Node, Path)], [(Node, Path)], [(Node, Node)], [(Node, Node)], Tree Node, Tree Node)
-- g1_expected = (
    -- [(1,[0]),(2,[1,0]),(3,[1,0]),(7,[1,0]),(4,[3,1,0]),(5,[4,3,1,0]),(6,[4,3,1,0])],
    -- [],
    -- [(7,1),(6,4),(4,3),(5,4),(3,1),(2,1),(1,0),(0,0)],[(0,0)],
    -- Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []}]}]},
    -- Node {rootLabel = 7, subForest = []}]}]},Node {rootLabel = 0, subForest = []})

-- main :: IO ()
-- main = do
--     let g0_result = applyDomFunctions (1,g0)
--     let g1_result = applyDomFunctions (0,g1)
--     if g0_result == g0_expected && g1_result == g1_expected
--         then exitWith ExitSuccess
--         else exitWith $ ExitFailure 1



