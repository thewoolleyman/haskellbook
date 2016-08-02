module Chapter7Exercises where

{-
  Multiple Choice:
  1. d
  2. b
  3. d
  4. b
  5. u
-}

tensDigit :: Integer -> Integer
tensDigit x = d
  where xLast = div x 10
        d     = mod xLast 10

tensDigit' :: Integer -> Integer
tensDigit' x = d
  where (xLast, _) = divMod x 10
        (_, d) = divMod xLast 10

tensDigitFnc :: Integer -> Integer
tensDigitFnc x = mod (fst (divMod x 10)) 10

tensDigitPF :: Integer -> Integer
tensDigitPF = (a . b . c)
  where applyToTen f x = f x 10
        a = applyToTen mod 
        b = fst
        c = applyToTen divMod 

applyTensDigit :: Integer -> Integer
applyTensDigit x = tensDigitPF x

hunsD :: Integer -> Integer
hunsD x = applyTensDigit (div x 10) 

foldBool :: a -> a -> Bool -> a
foldBool x y b
  | b == True   = x
  | b == False  = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b =
  case b of
    True -> x
    False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
