-- editorconfig-checker-disable-file
-- TODO: merge this module to Versions.hs ?
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
module Spec.Data.Eval (tests) where

import PlutusCore.Default
import PlutusCore.Evaluation.Machine.ExBudget
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults
import PlutusCore.MkPlc
import PlutusCore.Pretty
import PlutusCore.StdLib.Data.Unit
import PlutusCore.Version as PLC
import PlutusLedgerApi.Common
import PlutusLedgerApi.Common.Versions
import PlutusLedgerApi.Data.V1 qualified as V1
import PlutusLedgerApi.Data.V2 qualified as V2
import PlutusLedgerApi.Data.V3 qualified as V3
import PlutusLedgerApi.Test.V1.Data.EvaluationContext qualified as V1
import PlutusPrelude
import UntypedPlutusCore as UPLC
import UntypedPlutusCore.Test.DeBruijn.Bad
import UntypedPlutusCore.Test.DeBruijn.Good

import Control.Exception (evaluate)
import Control.Monad.Extra (whenJust)
import Control.Monad.Writer
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import NoThunks.Class
import Test.Tasty
import Test.Tasty.Extras (ignoreTestWhenHpcEnabled)
import Test.Tasty.HUnit

{- Note [Direct UPLC code]
For this test-suite we write the programs directly in the UPLC AST,
bypassing the GHC typechecker & compiler, the PIR typechecker & compiler and the PLC typechecker.
The reason is that users can submit such hand-crafted code, and we want to test how it behaves.
Because this is part of our API, we have to be careful not to change the behaviour of even weird untypeable programs.
In particular, we test the online part (API module).
-}

type T = Term DeBruijn DefaultUni DefaultFun ()

{-| Evaluates scripts as they will be evaluated on-chain, by using the evaluation function we provide for the ledger.
Notably, this goes via serialising and deserialising the program, so we can see any errors that might arise from that.
-}
testAPI :: TestTree
testAPI = "v1-api" `testWith` evalAPI vasilPV

evalAPI :: MajorProtocolVersion -> T -> Bool
evalAPI pv t =
    -- handcraft a serialised script
    let ss :: V1.SerialisedScript = V1.serialiseUPLC $ Program () PLC.plcVersion100 t
        s :: V1.ScriptForEvaluation = either (Prelude.error . show) id $ deserialiseScript PlutusV1 pv ss
        ec :: V1.EvaluationContext = fst $ unsafeFromRight $ runWriterT $ V1.mkEvaluationContext $ fmap snd V1.costModelParamsForTesting
    in isRight $ snd $ V1.evaluateScriptRestricting pv V1.Quiet ec (unExRestrictingBudget enormousBudget) s []

{-| Test a given eval function against the expected results.
These tests are modified from untyped-plutus-core-test:Evaluation.FreeVars
to accommodate the fact that the eval functions in the API
will do prior conformance checking (i.e. deserialization and scope checking).
-}
testWith :: String -> (T -> Bool) -> TestTree
testWith str evalFn = testGroup str $ fmap (uncurry testCase)
    [("delay0", evalFn (Delay () $ Var () $ DeBruijn 0) @?= False) -- fails at scopechecking
    ,("fun0var0", evalFn fun0var0 @?= False) -- fails at scopechecking
    ,("const0var0", evalFn (const0 @@ [unitval, fun0var0]) @?= False) -- fails at scopechecking
    ,("iteLazy0" , evalFn iteLazy0 @?= False) -- fails at scopechecking
    ,("iteStrict0", evalFn iteStrict0 @?= False) -- fails at scopechecking
--    ,("illITELazy", evalFn illITELazy @?= True) -- a type error that the machine cannot catch
--    ,("illITEStrict", evalFn illITEStrict @?= True) -- a type error that the machine cannot catch
--    ,("illAdd", evalFn illAdd @?= False) -- type error is caught by the machine
--    ,("illOverAppBuiltin", evalFn illOverAppBuiltin @?= False) -- type error is caught by the machine
--    ,("illOverAppFun", evalFn illOverAppFun @?= False) -- type error is caught by the machine
    ]

{- ** FIXME(https://github.com/IntersectMBO/plutus-private/issues/1611):

This is broken with the new cost model setup.

testUnlifting :: TestTree
testUnlifting = testCase "check unlifting behaviour changes in Vasil" $ do
    -- Before Vasil the behavior of this would return `False`, but since the behavior was never
    -- exercised on chain, it was safe to be switched to the new behavior (jedi mind trick).
    evalAPI alonzoPV illPartialBuiltin @?= True
    evalAPI vasilPV illPartialBuiltin @?= True
-}

costParams :: [Int64]
costParams = Map.elems (fromJust defaultCostModelParamsForTesting)

lengthParamNamesV :: PlutusLedgerLanguage -> Int
lengthParamNamesV PlutusV1 = length $ enumerate @V1.ParamName
lengthParamNamesV PlutusV2 = length $ enumerate @V2.ParamName
lengthParamNamesV PlutusV3 = length $ enumerate @V3.ParamName

mkEvaluationContextV :: PlutusLedgerLanguage -> IO EvaluationContext
mkEvaluationContextV ll =
    either (assertFailure . display) (pure . fst) . runWriterT $
        take (lengthParamNamesV ll) costParams & case ll of
            PlutusV1 -> V1.mkEvaluationContext
            PlutusV2 -> V2.mkEvaluationContext
            PlutusV3 -> V3.mkEvaluationContext

-- | Ensure that 'toMachineParameters' never throws for all language and protocol versions.
evaluationContextCacheIsComplete :: TestTree
evaluationContextCacheIsComplete =
    testGroup "EvaluationContext has machine parameters for all protocol versions" $
        enumerate <&> \ll -> testCase (show ll) $ do
            evalCtx <- mkEvaluationContextV ll
            for_ (futurePV:knownPVs) $ \pv ->
                evaluate $ toMachineParameters pv evalCtx

failIfThunk :: Show a => Maybe a -> IO ()
failIfThunk mbThunkInfo =
    whenJust mbThunkInfo $ \thunk ->
        assertFailure $ "Unexpected thunk: " <> show thunk

-- | Ensure that no 'EvaluationContext' has thunks in it for all language versions.
evaluationContextNoThunks :: TestTree
evaluationContextNoThunks =
    testGroup "NoThunks in EvaluationContext" $
        enumerate <&> \ll -> testCase (show ll) $ do
            !evalCtx <- mkEvaluationContextV ll
            failIfThunk =<< noThunks [] evalCtx

tests :: TestTree
tests = testGroup "eval"
    [ testAPI
--    , testUnlifting
    , evaluationContextCacheIsComplete
    , ignoreTestWhenHpcEnabled evaluationContextNoThunks
    ]

