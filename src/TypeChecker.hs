module TypeChecker where

import Language
import Util


typeCheck :: Translator L1 L2
typeCheck = L2Prog <$> ((,) <$> tcTypes <*> tcFuncs)

tcTypes :: Translator L1 [L2Type]
tcTypes = undefined

tcFuncs :: Translator L1 [L2Func]
tcFuncs = undefined
