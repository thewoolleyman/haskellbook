module ChangeMood where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood mood = Woot
changeMood    _ = Blah
