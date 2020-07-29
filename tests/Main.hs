{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Data.Graph.Dom as G
import Data.Tree
import System.Exit

import Data.List (nub, nubBy)
import Data.Containers.ListUtils
import Test.HUnit hiding (Node,Path)
-- import Test.Framework.Providers.QuickCheck2
-- import Test.QuickCheck (Property, (===))
-- import Test.QuickCheck.Function (Fun, apply)
-- import Test.QuickCheck.Poly (A, OrdA, B, OrdB, C)

-- main = defaultMain
--   [ test_dom
--   , testtest_pdom
--   , test_idom
--   , test_ipdom
--   , test_domTree
--   , test_pdomTree
--   ]

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
  counts <- runTestTT tests
  putStrLn $ showCounts counts
  if (errors counts + failures counts) > 0
    then exitWith $ ExitFailure 1
    else exitWith   ExitSuccess

tests = TestList $ map TestCase [dom0, dom1, pdom0, pdom1, ipdom0, ipdom1, dom_t_g0, dom_t_g1, dom_pt_g0, dom_pt_g1]

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



