module Main where

import Test.DocTest (doctest)

import Data.Graph

main :: IO ()
main = doctest ["-isrc"
               , "src/Data/Graph.hs"
               , "src/Data/Graph/InductiveGraph.hs"
               , "src/Data/Graph/Query.hs"
               , "src/Data/Heap.hs"
               ]
