module Util where

import Data.Bifunctor (second)

type SourceName = String

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
  translate :: SourceName -> a -> Either Error b
}

instance Functor (Translator a) where
  fmap f p = Translator $ (\translator srcname a -> second f $ translator srcname a) (translate p)
  
instance Applicative (Translator a) where
  pure a = Translator $ \_ _ -> Right a
  (<*>) a b = Translator $ \srcname inp -> apply (translate a srcname inp) (translate b srcname inp)
    where
      apply (Right resA) (Right resB) = Right (resA resB)
      apply (Left errA) _             = Left errA
      apply _           (Left errB)   = Left errB
