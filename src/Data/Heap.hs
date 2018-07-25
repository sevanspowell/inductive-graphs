-- Copyright (c) 1999-2008, Martin Erwig
--               2010, Ivan Lazar Miljenovic
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

-- 1. Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.

-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.

-- 3. Neither the name of the author nor the names of its contributors may be
--    used to endorse or promote products derived from this software without
--    specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE CPP #-}

-- | Pairing heap implementation of dictionary
module Data.Heap(
    -- * Type
    Heap(..),
    prettyHeap,
    printPrettyHeap,
    -- * Operations
    empty,unit,insert,merge,mergeAll,
    isEmpty,findMin,deleteMin,splitMin,
    build, toList, heapsort
) where

import Text.Show (showListWith)

data Heap a b = Empty | Node a b [Heap a b]
     deriving (Eq, Show, Read)

prettyHeap :: (Show a, Show b) => Heap a b -> String
prettyHeap = (`showsHeap` "")
  where
    showsHeap Empty             = id
    showsHeap (Node key val []) = shows key . (": "++) . shows val
    showsHeap (Node key val hs) = shows key . (": "++) . shows val
                                  .  (' ':) . showListWith showsHeap hs

printPrettyHeap :: (Show a, Show b) => Heap a b -> IO ()
printPrettyHeap = putStrLn . prettyHeap

----------------------------------------------------------------------
-- MAIN FUNCTIONS
----------------------------------------------------------------------

empty :: Heap a b
empty = Empty

unit :: a -> b -> Heap a b
unit key val = Node key val []

insert :: (Ord a) => (a, b) -> Heap a b -> Heap a b
insert (key, val) = merge (unit key val)

merge :: (Ord a) => Heap a b -> Heap a b -> Heap a b
merge h Empty = h
merge Empty h = h
merge h@(Node key1 val1 hs) h'@(Node key2 val2 hs')
    | key1<key2 = Node key1 val1 (h':hs)
    | otherwise = Node key2 val2 (h:hs')

mergeAll:: (Ord a) => [Heap a b] -> Heap a b
mergeAll []        = Empty
mergeAll [h]       = h
mergeAll (h:h':hs) = merge (merge h h') (mergeAll hs)

isEmpty :: Heap a b -> Bool
isEmpty Empty = True
isEmpty _     = False

findMin :: Heap a b -> (a, b)
findMin Empty      = error "Heap.findMin: empty heap"
findMin (Node key val _) = (key, val)

deleteMin :: (Ord a) => Heap a b -> Heap a b
deleteMin Empty             = Empty
deleteMin (Node _ _ hs) = mergeAll hs

splitMin :: (Ord a) => Heap a b -> (a,b,Heap a b)
splitMin Empty             = error "Heap.splitMin: empty heap"
splitMin (Node key val hs) = (key,val,mergeAll hs)


----------------------------------------------------------------------
-- APPLICATION FUNCTIONS, EXAMPLES
----------------------------------------------------------------------


build :: (Ord a) => [(a,b)] -> Heap a b
build = foldr insert Empty

toList :: (Ord a) => Heap a b -> [(a,b)]
toList Empty = []
toList h = x:toList r
           where (x,r) = (findMin h,deleteMin h)

heapsort :: (Ord a) => [a] -> [a]
heapsort = map fst . toList . build . map (\x->(x,x))
{-
l :: (Num a) => [a]
l  = [6,9,2,13,6,8,14,9,10,7,5]
l' = reverse l

h1  = build $ map (\x->(x,x)) l
h1' = build $ map (\x->(x,x)) l'

s1  = heapsort l
s1' = heapsort l'
-}
