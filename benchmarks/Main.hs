module Main(main) where

import Data.Graph.Dom
import Control.DeepSeq
import Criterion.Main

g0 :: Rooted
g0 = (1,
      fromAdj
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
    )

g1 :: Rooted
g1 = (0,
       fromAdj
        [(0,[1])
        ,(1,[2,3])
        ,(2,[7])
        ,(3,[4])
        ,(4,[5,6])
        ,(5,[7])
        ,(6,[4])
        ,(7,[])]
    )

-- Our benchmark harness.
main :: IO ()
main = g0 `deepseq` g1 `deepseq`
    defaultMain [
        bgroup "g0" [ bench "dom"       $ nf dom g0
                    , bench "pdom"      $ nf pdom g0
                    , bench "idom"      $ nf idom g0
                    , bench "ipdom"     $ nf ipdom g0
                    , bench "domTree"   $ nf domTree g0
                    , bench "pdomTree"  $ nf pdomTree g0
            ],
        bgroup "g1" [ bench "dom"       $ nf dom g1
                    , bench "pdom"      $ nf pdom g1
                    , bench "idom"      $ nf idom g1
                    , bench "ipdom"     $ nf ipdom g1
                    , bench "domTree"   $ nf domTree g1
                    , bench "pdomTree"  $ nf pdomTree g1
            ]
        ]