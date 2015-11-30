> import Control.Monad

Reverse apply

 (>$>)	:: a -> (a -> b) -> b
 x >$> f	= f x	-- f $ x 

 (>.>) :: (a -> b) -> (b -> c) -> (a -> c)
 f >.> g = g .f

 j :: a->a
 j x = x

 (>=>) :: (a-> m b) -> (b -> m c) -> (a -> mc)
 f >=> g = \x -> (f x >= g)

 f :: Int -> Maybe Int
 f x = if x `mod` 2 == 0 then Nothing else Just (2*x)

 g :: Int -> Maybe Int
 g x = if x `mod` 3 == 0 then Nothing else Just (3*x)

 h :: Int -> Maybe Int
 h x = if x `mod` 5 == 0 then Nothing else Just (5*x)

 k :: Int -> Maybe Int
 k = f >=> g >=> h

or: 

k x = f x >>= g >>= h

Why?

--k = f >=> g >=> h
--	(f >=> g) >=> h
--	(\x -> f x >>= g) >=> h	--definition of >=>
--	let w = \x -> f x >>= h
--	w >=> h
--	\y -> w y >>= h 		--definition of >=>
-- k y = w y >>= h			--definition of lambda function ... f = \x -> x+2 <==> f x = x+2
-- k y = (\x -> f x >>= g) y >>= h	--expand w
-- k y = f y >>= g >>=	h		--apply w to the value y... beta reduction
-- k x = f x >>= g >>= h		--renaming y


or:

k x = do y <- f x
	 z <- g y
	 h z


