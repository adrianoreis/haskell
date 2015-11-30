http://mvanier.livejournal.com/5103.html

 import Control.Monad

> data ArithmeticError = DivideByZero | NotDivisible deriving Show

> safe_divide 		:: Int -> Int -> Either ArithmeticError Int
> safe_divide _ 0 		= Left DivideByZero
> safe_divide i j
>	| i `mod ` j /= 0	= Left NotDivisible
> safe_divide i j		= Right (i `div` j)

> divide	:: Int -> Int -> Either ArithmeticError Int
> divide i j	= case i `safe_divide` j of
>			Left DivideByZero -> Left DivideByZero
>			Left NotDivisible -> Right (i `div` j)
>			Right k		  -> Right k

Error handling monad

 instance Monad (Either e) where
 Left x  >= f 	= Left x
 Right x >= f 	= f x
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
