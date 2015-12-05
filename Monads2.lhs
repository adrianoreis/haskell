http://mvanier.livejournal.com/5103.html

> {-# LANGUAGE FlexibleContexts #-}

> import Control.Monad.Error

> data ArithmeticError = 
>		DivideByZero |
>		NotDivisible Int Int 
>		deriving Show

> safe_divide 		:: Int -> Int -> Either ArithmeticError Int
> safe_divide _ 0 		= throwError DivideByZero
> safe_divide i j
>	| i `mod ` j /= 0	= throwError (NotDivisible i j)
> safe_divide i j		= return (i `div` j)

 divide	:: Int -> Int -> Either ArithmeticError Int
 divide i j	= case i `safe_divide` j of
			Left DivideByZero -> Left DivideByZero
			Left NotDivisible -> Right (i `div` j)
			Right k		  -> Right k

data Either a b = Left a | Right b

Error handling monad

 instance Monad (Either e) where
 Left x  >>= f 	= Left x
 Right x >>= f 	= f x
 return x	= Right x

without Either e monad

> g		:: Int -> Int -> Int -> Either ArithmeticError Int
> g i j k	= 
> 	case i `safe_divide` k of
>		Left err1 -> Left err1
>		Right q1  ->
>			case j `safe_divide` k of
>			 Left err2 -> Left err2
>			 Right q2  -> Right (q1 + q2)

Using Either e monad

> g'		:: Int -> Int -> Int -> Either ArithmeticError Int
> g' i j k	=
>	do
>	 x <- i `safe_divide` k
>	 y <- j `safe_divide` k
>	 return (x + y)


g' i j k = i `safe_divide` k >>= \x -> j `safe_divide` k >>= \y -> return (x + y)



 instance MonadError ArithmeticError (Either ArithmeticError) where
 throwError err 		= Left err
 catchError (Left err) h	= h err 

> arithmeticErrorHandler :: ArithmeticError -> Either ArithmeticError Int
> arithmeticErrorHandler  DivideByZero = throwError DivideByZero
> arithmeticErrorHandler (NotDivisible i j)  = return (i `div` j)

> divide :: Int -> Int -> Either ArithmeticError Int

divide i j = catchError (i `safe_divide` j) arithmeticErrorHandler

 divide i j = (i `safe_divide` j) `catchError` handler
	where
	 handler :: ArithmeticError -> Either ArithmeticError Int
	 handler DivideByZero		= throwError DivideByZero
	 handler (NotDivisible i j) 	= return (i `div` j)

> divide i j = (i `safe_divide` j) `catchError` \e ->
>						case e of
>						  DivideByZero		-> throwError DivideByZero
>						  NotDivisible i j	-> return (i `div` j)
