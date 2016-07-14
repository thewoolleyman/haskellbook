module TypeExercises59 where

-- i :: a -> a
-- i q = q
--
-- c :: a -> b -> a
-- c q w = q
--
-- c'' :: b -> a -> b
-- c'' e r = e
--
-- c' :: a -> b -> b
-- c' z x = x


-- r :: [a] -> [a]
-- r c = c

co :: (b -> c) -> (a -> b) -> (a -> c) -- ; co = undefined
--co :: (b -> c) -> (a -> b) -> a -> c -- ; co = undefined
co b2c a2b x = b2c (a2b x)
-- co b2c a2b = \x -> b2c (a2b x)
-- co b2c a2b = b2c . a2b

-- co' :: (a -> b)
-- co' x = 1

-- co'' :: a -> b
-- co'' x y = (x, y)