module Evaluate where

{-

  const 1 undefined =
    \a -> \b -> a
    \1 -> \_ -> 1
    1

  const undefined 1 =
    \a -> \b -> a
    \undefined -> \_ -> undefined
    undefined

  flip const undefined 1 =
     (\a -> \b -> c) -> \b -> \a -> c
     let b = undefined, a = 1
       in (\a -> \_ -> a) 
      \1 -> \_ -> 1
      1

  flip const 1 undefined =
    (\a -> \b -> c) -> \b -> \a -> c
    let b = 1, a = undefined
      in (\a -> \_ -> a)
    \undefined -> \_ -> undefined
    undefined

  const undefined undefined =
    \a -> \b -> a
    \undefined -> \undefined -> undefined
    undefined

  foldr const 'z' ['a'..'e'] =
    (a -> b -> b) -> b -> t a -> b 
    (\a -> \_ -> a) -> \'z' -> ['a'..'e'] -> b
    let b = 'z', a = ['a'..'e']
      in (\a -> \_ -> a)

    let b = 'z', a = 'a'
      in (\'a' -> \_ -> 'a'
    'a'

  foldr (flip const) 'z' ['a'..'e'] =
    (a -> b -> b) -> b -> t a -> b
    (let b = 'z', a = ['a'..'e']
      in (\b -> \a -> \b) -> \b -> \a -> b)

    'z'
-}
