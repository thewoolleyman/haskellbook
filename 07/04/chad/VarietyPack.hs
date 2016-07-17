module VarietyPack where

-- ex 1

k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- 1a:
--  (t, t1) => t
-- 1b:
--  String or [Char].  No
-- 1c:
--  k1 and k3

-- ex 2

func :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
func (a, b, c) (d, e, f) = ((a, d), (c, f))
