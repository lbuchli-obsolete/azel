{-# LANGUAGE MultiParamTypeClasses #-}
module Lib (
  SourceName,
  Translate, translate,
  L0, source,
  L1, L2
) where

import Text.Parsec (parse, ParseError)
import Data.Bifunctor
import Util
import Parser
import TypeChecker
import Language

-- translate one level to another
class Show e => Translate a e b where
  translate :: SourceName -> a -> Either e b

instance Translate L0 ParseError L1 where
  translate srcname (L0Prog src) = parse parseProg srcname src
  
instance Translate L0 Error L2 where
  translate srcname src = flatten $ bimap (Error . show) (typeCheck srcname) (translate srcname src :: Either ParseError L1)
