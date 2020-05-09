{-# LANGUAGE MultiParamTypeClasses #-}
module Lib (
  SourceName,
  Error,
  Translator(..),
  L0, L1, L2,
  source, parseSource, typeCheck
) where

import Util
import Language
import Parser
import TypeChecker
