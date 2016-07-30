module Chapter7Exercises where

{-
  Multiple Choice:
  1. d
  2. b
  3. d
  4. b
  5. u
-}

tensDigit x = d
  where xLast = div x 10
        d     = mod xLast 10

tensDigit' x = mod (fst (divMod x 10)) 10

tensDigitPF = (a . b . c)
  where applyToTen f x = f x 10
        a = applyToTen mod 
        b = fst
        c = applyToTen divMod 

applyTensDigit x = tensDigitPF x
