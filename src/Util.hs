module Util where

import Data.Bifunctor (second)

newtype Error = Error String
  deriving Show

flatten :: Either a (Either a b) -> Either a b
flatten (Left a)          = Left a
flatten (Right (Left a))  = Left a
flatten (Right (Right b)) = Right b


---------------------------------------------------
--                     Parser                    --
---------------------------------------------------

newtype Translator a b = Translator {
  translate :: a -> Either Error b
}

instance Functor (Translator a) where
  fmap f p = Translator $ (\translator a -> second f $ translator a) (translate p)
  
instance Applicative (Translator a) where
  pure a = Translator $ \_ -> Right a
  (<*>) a b = Translator $ \inp -> apply (translate a inp) (translate b inp)
    where
      apply (Right resA) (Right resB) = Right (resA resB)
      apply (Left errA) _             = Left errA
      apply _           (Left errB)   = Left errB
