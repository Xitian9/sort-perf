module Sorts.New (sort, sortBy, sortOn, sortWith) where

{-# INLINEABLE sort #-}
sort :: Ord a => [a] -> [a]
sort = actualSort (>)

{-# INLINEABLE sortBy #-}
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = actualSort (\x y -> cmp x y == GT)

{-# INLINE actualSort #-}
actualSort :: (a -> a -> Bool) -> [a] -> [a]
actualSort gt ns
  | []        <- ns = []
  | [a]       <- ns = [a]
  | [a,b]     <- ns = merge [a] [b]
  | [a,b,c]   <- ns = merge3 [a] [b] [c]
  | [a,b,c,d] <- ns = merge4 [a] [b] [c] [d]
  | otherwise       = merge_all (sequences ns)
  where
    sequences (a:b:xs)
      | a `gt` b  = descending b [a]  xs
      | otherwise = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `gt` b       = descending b (a:as) bs
    descending a as bs = (a:as): sequences bs

    ascending a as (b:bs)
      | not (a `gt` b) = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs  = let !x = as [a]
                         in x : sequences bs

    merge_all [x] = x
    merge_all xs  = merge_all (reduce_once xs)

    reduce_once []            = []
    reduce_once [a]           = [a]
    reduce_once [a,b]         = [merge a b]
    reduce_once [a,b,c]       = [merge3 a b c]
    reduce_once [a,b,c,d,e]   = [merge a b, merge3 c d e]
    reduce_once [a,b,c,d,e,f] = [merge3 a b c, merge3 d e f]
    reduce_once (a:b:c:d:xs)  = let !x = merge4 a b c d
                                in x : reduce_once xs

    merge as@(a:as') bs@(b:bs')
      | a `gt` b  = b : merge as  bs'
      | otherwise = a : merge as' bs
    merge [] bs   = bs
    merge as []   = as

    -- `merge3` is a manually fused version of `merge (merge as bs) cs`
    merge3 as@(a:as') bs@(b:bs') cs
      | a `gt` b  = merge3X b as  bs' cs
      | otherwise = merge3X a as' bs  cs
    merge3 [] bs cs = merge bs cs
    merge3 as [] cs = merge as cs

    merge3X x as bs cs@(c:cs')
      | x `gt` c  = c : merge3X x as bs cs'
      | otherwise = x : merge3    as bs cs
    merge3X x as bs [] = x : merge as bs

    merge3Y as@(a:as') y bs cs
      | a `gt` y  = y : merge3  as    bs cs
      | otherwise = a : merge3Y as' y bs cs
    merge3Y [] x bs cs = x : merge bs cs

    -- `merge4 as bs cs ds` is (essentially) a manually fused version of
    -- `merge (merge as bs) (merge cs ds)`
    merge4 as@(a:as') bs@(b:bs') cs ds
      | a `gt` b  = merge4X b as  bs' cs ds
      | otherwise = merge4X a as' bs  cs ds
    merge4 [] bs cs ds = merge3 bs cs ds
    merge4 as [] cs ds = merge3 as cs ds

    merge4X x as bs cs@(c:cs') ds@(d:ds')
      | c `gt` d  = merge4XY x as bs d cs  ds'
      | otherwise = merge4XY x as bs c cs' ds
    merge4X x as bs [] ds = merge3X x as bs ds
    merge4X x as bs cs [] = merge3X x as bs cs

    merge4Y as@(a:as') bs@(b:bs') y cs ds
      | a `gt` b  = merge4XY b as  bs' y cs ds
      | otherwise = merge4XY a as' bs  y cs ds
    merge4Y as [] y cs ds = merge3Y as y cs ds
    merge4Y [] bs y cs ds = merge3Y bs y cs ds

    merge4XY x as bs y cs ds
      | x `gt` y  = y : merge4X x as bs   cs ds
      | otherwise = x : merge4Y   as bs y cs ds

{-# INLINEABLE sortOn #-}
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . actualSort (\x y -> fst x > fst y) . map (\x -> let y = f x in y `seq` (y, x))

{-# INLINEABLE sortWith #-}
sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = actualSort (\x y -> f x > f y)
