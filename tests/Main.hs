module Main (main) where

import Data.Graph.Dom as G
import Data.Tree
import System.Exit

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

applyDomFunctions :: Rooted
                  -> ([(Node, Path)], [(Node, Path)], [(Node, Node)], [(Node, Node)], Tree Node, Tree Node)
applyDomFunctions g = (dom g, pdom g, idom g, ipdom g, domTree g, pdomTree g)

g0_expected :: ([(Node, Path)], [(Node, Path)], [(Node, Node)], [(Node, Node)], Tree Node, Tree Node)
g0_expected = (
  [(2,[1]),(3,[1]),(4,[3,1]),(5,[4,3,1]),(6,[4,3,1]),(7,[4,3,1]),(8,[7,4,3,1]),(9,[8,7,4,3,1]),(10,[8,7,4,3,1])],
  [(9,[1]),(8,[9,1]),(7,[8,9,1]),(4,[7,8,9,1]),(5,[7,8,9,1]),(6,[7,8,9,1]),(10,[7,8,9,1]),(3,[4,7,8,9,1]),(2,[3,4,7,8,9,1])],
  [(10,8),(7,4),(9,8),(1,1),(8,7),(3,1),(4,3),(6,4),(5,4),(2,1)],
  [(10,7),(8,9),(9,1),(7,8),(6,7),(5,7),(4,7),(3,4),(2,3),(1,1)],
  Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []},Node {rootLabel = 7, subForest = [Node {rootLabel = 8, subForest = [Node {rootLabel = 9, subForest = []},Node {rootLabel = 10, subForest = []}]}]}]}]}]},
  Node {rootLabel = 1, subForest = [Node {rootLabel = 9, subForest = [Node {rootLabel = 8, subForest = [Node {rootLabel = 7, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 3, subForest = [Node {rootLabel = 2, subForest = []}]}]},Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []},Node {rootLabel = 10, subForest = []}]}]}]}]}
  )

g1_expected :: ([(Node, Path)], [(Node, Path)], [(Node, Node)], [(Node, Node)], Tree Node, Tree Node)
g1_expected = (
    [(1,[0]),(2,[1,0]),(3,[1,0]),(7,[1,0]),(4,[3,1,0]),(5,[4,3,1,0]),(6,[4,3,1,0])],
    [],[(7,1),(6,4),(4,3),(5,4),(3,1),(2,1),(1,0),(0,0)],[(0,0)],
    Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 5, subForest = []},Node {rootLabel = 6, subForest = []}]}]},
    Node {rootLabel = 7, subForest = []}]}]},Node {rootLabel = 0, subForest = []})

main :: IO ()
main = do
    let g0_result = applyDomFunctions (1,g0)
    let g1_result = applyDomFunctions (0,g1)
    if g0_result == g0_expected && g1_result == g1_expected
        then exitWith ExitSuccess
        else exitWith $ ExitFailure 1



