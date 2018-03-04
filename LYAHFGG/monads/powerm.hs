--from Stack Overflow

{-
from the standard library the Monad instance for Either is

instance Monad (Either a) where
    return = Right
    Left a >>= _ = Left a
    Right b >>= f = f b
-}

-- we want to implement a function such that the following is true:
-- ifA (pure True) t e == t
-- ifA (pure False) t e == e

--using the monad it is possible
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mbool tr fa = do
    bool <- mbool
    if bool then tr else fa
    
--using the applicative it is not possible
ifA :: Applicative f => f Bool -> f a -> f a -> f a
ifA fbool tr fa = fmap <$> (\x -> if x then tr else fa) <*> fbool

--the type of x is Bool but must pass a f Bool
--the return type of ifA is f a from the signature but f (f a) from the implementation
--not possible without using the join function which is defined in the monads module
--(for generic applicatives)
    
