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

type Parser a b = a -> Either Error b

(.&.) :: Parser a b -> Parser b c -> Parser a c
(.&.) pA pB inp = flatten $ second pB (pA inp)

(.|.) :: Parser a b -> Parser a b -> Parser a b
(.|.) pA pB inp = case pA inp of
  Left err -> case pB inp of
    Left _ -> Left err
    Right res -> Right res
  Right res -> Right res

(.$.) :: (b -> c) -> Parser a b -> Parser a c
(.$.) f p inp = second f (p inp)

(.*.) :: Parser a b -> Parser a c -> Parser a (b -> c)
(.*.) = undefined
