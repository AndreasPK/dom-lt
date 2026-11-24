module Main(main) where

import Data.Graph.Dom
import Control.DeepSeq
import Criterion.Main
import Data.Array (Array, elems)

import Loader (loadGraphs)
import GHC.IO (evaluate)

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

g_star :: Rooted
g_star = star 20000
    where
        star :: Int -> Rooted
        star n = (0, fromAdj $ (0, [1..n-1]) : [(i, []) | i <- [1..n-1]])

-- CFGs taken from a complete GHC build (GHC + boot libraries).
-- Deduplicated by shape and larger graphs sampled by keeping every 8th
cfgFile :: FilePath
cfgFile = "benchmarks/ghc_cfgs.gz"

-- idom and dom on every graph of a group once.
ghcCfgGroup :: String -> Array Int Rooted -> Benchmark
ghcCfgGroup name gs =
    bgroup name [ bench "idom" $ nf (map idom . elems) gs
                , bench "dom"  $ nf (map dom  . elems) gs
                ]

-- Our benchmark harness.
main :: IO ()
main = do
    ghc_graphs@((small_cfgs, g_ghc_lower_80), (mid_cfg_samples, g_ghc_80_to_95), (large_cfg_samples, g_ghc_95_to_100)) <- loadGraphs cfgFile
    _ <- evaluate $ force (g0,g1,g_star,ghc_graphs)
    putStrLn $
        "Loaded representative GHC CFG samples:\n" ++
        (show $ length g_ghc_lower_80) ++ " small graph shapes representing all " ++ (show $ small_cfgs) ++ "of the smallest 80% of CFGs.\n" ++
        -- I've kept every 8th CFG shape out of all unique shapes.
        (show $ length g_ghc_80_to_95) ++ " medium sized shapes representing the 80%-95% size range, taken from " ++ (show $ mid_cfg_samples) ++ " CFGs.\n" ++
        (show $ length g_ghc_95_to_100) ++ " large shapes representing the largest 5% of cfgs, taken from " ++ (show $ large_cfg_samples) ++ " CFGs.\n"

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
            ],
        bgroup "star-20000" [ bench "idom" $ nf idom (g_star)
                            , bench "dom"  $ nf dom (g_star)
            ],
        ghcCfgGroup "ghc-lower-80"  g_ghc_lower_80,
        ghcCfgGroup "ghc-80-to-95"  g_ghc_80_to_95,
        ghcCfgGroup "ghc-95-to-100" g_ghc_95_to_100
        ]
