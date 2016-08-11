module PhoneExercise where

type Symbol = Char
type Letters = [Char]

data Button
  = Button Symbol Letters

data Input
  = Input Button Int

data DaPhone = DaPhone [Button]

defaultPhone :: DaPhone
defaultPhone =
  DaPhone [ Button '1' []
          , Button '2' ['a', 'b', 'c']
          , Button '3' ['d', 'e', 'f']
          ]


charToNum :: DaPhone -> Char -> [Int]
charToNum phone char = undefined
  --foldr (doesMatch char) 

--data Button
--  = Button Symbol [A, B, C]

-- type alphabet = A | B | C ...


-- one =
--   Button '2' [('A', 'a'), ('B', 'b'), ('C', 'c')]

-- pressesToLetter input =
--  case input of
--    ('2', 1) -> 'a'
--    ('2', 2) -> 'b' 
--    ...


