> data State s a = State (s -> (a,s))

> type GCDState = (Int, Int)

> gcd_s1' :: State GCDState Int
> gcd_s1' = State (\s ->
>			let (x,y) = s in
>		 case compare x y of
>			 	EQ -> (x, (x,y))
>			 	LT -> f (y,x)
>			 	GT -> f (x, x-y))
>		where
>		f :: GCDState -> (Int, GCDState)
>		f = runState gcd_s1'

> runState :: State s a -> s -> (a,s)
> runState (State f) init_st = f init_st

> evalState :: State s a -> s -> a
> evalState mv init_st = fst (runState mv init_st)

> execState :: State s a -> s -> s
> execState mv init_st = snd (runState mv init_st)

> run_gcd_s1'::Int -> Int -> Int
> run_gcd_s1' x y = fst (runState gcd_s1' (x,y))

