{-# LANGUAGE MultiParamTypeClasses #-}
module Lib (
  SourceName,
  L0, source,
  L1, L2
) where

import Text.Parsec (parse, ParseError)
import Data.Bifunctor
import Util
import Parser
import TypeChecker
import Language
