import Prelude hiding (Maybe(..), fail)

import Control.Monad.Fail (MonadFail(..))

data Maybe a = Nothing
             | Just a
  deriving (Eq, Ord, Show)

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure x = Just x

    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    Just f  <*> x = fmap f x

instance Monad Maybe where
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    Just x  >>= f = f x

instance MonadFail Maybe where
    fail _ = Nothing

data Exp = Val Int
         | Add Exp Exp
         | Div Exp Exp
         | Trace Exp
  deriving (Eq, Ord, Show)

eval :: Exp -> IO Int
eval (Val i)     = return i
eval (Add e1 e2) = do x <- eval e1
                      y <- eval e2
                      return (x + y)

eval (Div e1 e2) = do x <- eval e1
                      y <- eval e2
                      if y == 0 then fail "division by zero" else return (x `div` y)

eval (Trace e)   = do x <- eval e
                      print x
                      return x
