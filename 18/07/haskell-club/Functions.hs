module Functions where

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma >>= (<$> mb)

a :: Monad m => m a -> m (a -> b) -> m b
--a ma mf = mf >>= (\f -> ma >>= (\m -> return $ f m))
a = (=<<) . flip fmap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as f = foldr (l2 (:)) (return []) (f <$> as)

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' as f = flipType' $ fmap f as

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id

flipType' :: (Monad m) => [m a] -> m [a]
flipType' = foldr (l2 (:)) (return [])

