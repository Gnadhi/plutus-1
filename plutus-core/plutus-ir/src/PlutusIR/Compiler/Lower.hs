{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module PlutusIR.Compiler.Lower where

import PlutusIR
import PlutusIR.Compiler.Types
import PlutusIR.Error

import PlutusCore qualified as PLC

import Control.Monad.Except

-- | Turns a PIR 'Term' with no remaining PIR-specific features into a PLC 'PLC.Term' by simply
-- translating the constructors across.
lowerTerm :: Compiling m uni fun a => PIRTerm uni fun a -> m (PLCTerm uni fun a)
lowerTerm = \case
    Let x _ _ _      -> throwError $
        CompilationError x "Let bindings should have been eliminated before lowering"
    Var x n          -> pure $ PLC.Var x n
    TyAbs x n k t    -> PLC.TyAbs x n k <$> lowerTerm t
    LamAbs x n ty t  -> PLC.LamAbs x n ty <$> lowerTerm t
    Apply x t1 t2    -> PLC.Apply x <$> lowerTerm t1 <*> lowerTerm t2
    Constant x c     -> pure $ PLC.Constant x c
    Builtin x bi     -> pure $ PLC.Builtin x bi
    TyInst x t ty    -> PLC.TyInst x <$> lowerTerm t <*> pure ty
    Error x ty       -> pure $ PLC.Error x ty
    IWrap x tn ty t  -> PLC.IWrap x tn ty <$> lowerTerm t
    Unwrap x t       -> PLC.Unwrap x <$> lowerTerm t
    Constr x ty i es -> PLC.Constr x ty i <$> traverse lowerTerm es
    Case x ty arg cs -> PLC.Case x ty <$> lowerTerm arg <*> traverse lowerTerm cs
