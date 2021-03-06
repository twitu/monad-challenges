{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import           MCPrelude
import           Set4

----------------------------------------
-- Part 1
----------------------------------------

-- moved to `Set4.hs`

----------------------------------------
-- Part 2
----------------------------------------

headMay :: [a] -> Maybe a
headMay []      = Nothing
headMay (a : _) = Just a

tailMay :: [a] -> Maybe [a]
tailMay []       = Nothing
tailMay (_ : as) = Just as

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ []            = Nothing
lookupMay x ((k, b) : vs) = if x == k then Just b else lookupMay x vs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _   0     = Nothing
divMay num denum = Just $ num / denum

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []  = Nothing
maximumMay [a] = Just a
maximumMay as  = Just $ foldl1 (max) as

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []  = Nothing
minimumMay [a] = Just a
minimumMay as  = Just $ foldl1 (min) as

----------------------------------------
-- Part 3
----------------------------------------

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gdata key = do
  val <- lookupMay key gdata
  ts <- tailMay val
  mxv <- maximumMay ts
  hdv <- headMay val
  ans <- divMay (fromIntegral mxv) (fromIntegral hdv)
  return ans

----------------------------------------
-- Part 4
----------------------------------------

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f (Just v) = f v
chain _ Nothing  = Nothing

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link (Just v) f = f v
link Nothing  _ = Nothing

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gdata key =
  let val = lookupMay key gdata
      mxv = link (link (link val tailMay) maximumMay) (Just . fromIntegral)
      hdv = link (link val headMay) (Just . fromIntegral)
      ans = link mxv (\numer -> link hdv (divMay numer))
  in  ans

----------------------------------------
-- Part 5
----------------------------------------

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries info k1 k2 = do
  v1 <- lookupMay k1 info
  v2 <- lookupMay k2 info
  return $ v1 + v2

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb = link (ma) (\v1 -> link (mb) (Just . f v1))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 info k1 k2 = yLink (+) (lookupMay k1 info) (lookupMay k2 info)

mkMaybe :: a -> Maybe a
mkMaybe = Just

----------------------------------------
-- Part 6
----------------------------------------

tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
  vals <- tailMay xs
  return $ product vals

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
  vals <- tailMay xs
  return $ sum vals

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f (Just x) = mkMaybe . f $ x
transMaybe _ Nothing  = Nothing

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do
  vals <- tailMay xs
  maxv <- maximumMay vals
  return maxv

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = combine $ transMaybe (minimumMay) (tailMay xs)

combine :: Maybe (Maybe a) -> Maybe a
combine (Just (Just a)) = mkMaybe a
combine _               = Nothing
