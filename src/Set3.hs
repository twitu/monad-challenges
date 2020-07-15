{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude


--------------------------------------------------------
-- Part 1
--------------------------------------------------------

allPairs :: [a] -> [b] -> [(a, b)]
allPairs []       _ = []
allPairs [_] [] = []
allPairs [a] (x:xs) = (a, x) : allPairs [a] xs
allPairs (a:as) xs = allPairs [a] xs ++ allPairs as xs

--------------------------------------------------------
-- Part 2
--------------------------------------------------------

data Card = Card Int String

instance Show Card where
  show (Card n c) = show n ++ c

instance Eq Card where
  (==) (Card n1 c1) (Card n2 c2) = n1 == n2 && c1 == c2

allCards :: [Int] -> [String] -> [Card]
allCards []       _ = []
allCards [_] [] = []
allCards [a] (x:xs) = Card a x : allCards [a] xs
allCards (a:as) xs = allCards [a] xs ++ allCards as xs

--------------------------------------------------------
-- Part 3
--------------------------------------------------------

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ []       _ = []
allCombs _ [_] [] = []
allCombs f [a] (x:xs) = f a x : allCombs f [a] xs
allCombs f (a:as) xs = allCombs f [a] xs ++ allCombs f as xs

allPairs' :: [a] -> [b] -> [(a, b)]
allPairs' = allCombs (,)

allCards' :: [Int] -> [String] -> [Card]
allCards' = allCombs Card

--------------------------------------------------------
-- Part 3
--------------------------------------------------------

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 _ [_] [] _ = []
allCombs3 _ [_] [_] [] = []
allCombs3 f [a] [b] (c:cs) = f a b c : allCombs3 f [a] [b] cs
allCombs3 f [a] (b:bs) cs = allCombs3 f [a] [b] cs ++ allCombs3 f [a] bs cs
allCombs3 f (a:as) bs cs  = allCombs3 f [a] bs cs ++ allCombs3 f as bs cs

--------------------------------------------------------
-- Part 4
--------------------------------------------------------

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep [_] [] = []
combStep [f] (x:xs) = f x : combStep [f] xs
combStep (f:fs) xs = combStep [f] xs ++ combStep fs xs

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f as bs = combStep (combStep [f] as) bs

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f as bs cs = combStep (combStep (combStep [f] as) bs ) cs

