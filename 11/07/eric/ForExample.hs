module ForExample where

data Example = MakeExample deriving Show
data AnotherExample = MakeExample2 Int deriving Show

-- :t MakeExample = Example
-- :t Example = Data constructor not in scope: Example

-- :info MakeExample = :data Example = MakeExample     -- Defined at ForExample.hs:3:16
-- :info Example = data Example = MakeExample      -- Defined at ForExample.hs:3:1
--                 instance [safe] Show Example -- Defined at ForExample.hs:3:37

-- :t MakeExample2 = MakeExample2 :: Int -> AnotherExample
-- :t AnotherExample = Data constructor not in scope: AnotherExample
