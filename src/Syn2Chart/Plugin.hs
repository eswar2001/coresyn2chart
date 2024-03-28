{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use init" #-}

module Syn2Chart.Plugin (plugin) where

import Syn2Chart.Types ( LBind(LRec, LNonRec), LAltCon(..), LExpr(..), bindToJSON, CaseExtract (..) )
import CoreMonad ( liftIO, CoreM, CoreToDo(CoreDoPluginPass) )
import CoreSyn
    ( AltCon(..),
      Expr(Case, Lit, Type, Var, App, Lam, Let),
      CoreBind,
      Bind(Rec, NonRec),
      CoreExpr )
import Data.Aeson ( ToJSON(toJSON), encode )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as DBS
import Data.Data ( Data(toConstr) )
import Control.Monad (unless)
import GhcPlugins
    ( Plugin(installCoreToDos, pluginRecompile),
      unpackFS,
      idName,
      coVarDetails,
      noCafIdInfo,
      mkLitString,
      moduleNameString,
      mkInternalName,
      nameStableString,
      mkVarOcc,
      showSDocUnsafe,
      defaultPlugin,
      purePlugin,
      mkLocalVar,
      tyVarKind,
      ModGuts(mg_binds, mg_module, mg_loc),
      Module(moduleName),
      Outputable(ppr),
      CommandLineOption, Var, NamedThing (getName), Literal (..), FunctionOrData (..), LitNumType (..), showSDoc )
import Prelude hiding (id)
import Data.Text.Encoding (decodeUtf8)
import Data.Text ( unpack, Text, pack, isInfixOf, append )
import Data.List.Extra (replace,intercalate,splitOn,isSuffixOf)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Map as Map
import System.Directory (createDirectoryIfMissing)
import Unique ( mkUnique )
import Name (getSrcSpan)
import Var (isLocalId)
import Id (isExportedId)
import SrcLoc
import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Data.Time
import Debug.Trace (trace)
import Control.Concurrent (MVar, newMVar, modifyMVar)
import GHC.IO (unsafePerformIO)
import Data.Int (Int64)

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
    , pluginRecompile = GhcPlugins.purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args todos = return (CoreDoPluginPass "CoreSyn2Chart" (buildCfgPass args) : todos)

buildCfgPass ::  [CommandLineOption] -> ModGuts -> CoreM ModGuts
buildCfgPass opts guts = do
    let prefixPath = case opts of
                        []    -> "/tmp/coresyn2chart/"
                        [local] -> local
                        _ -> error "unexpected no of arguments"
    _ <- liftIO $ do
        let binds = mg_binds guts
            moduleN = moduleNameString $ moduleName $ mg_module guts
            moduleLoc = prefixPath Prelude.<> getFilePath (mg_loc guts)
        unless ("Types.hs" `isSuffixOf` moduleN || "EC." `isInfixOf` pack moduleN) $ do
            createDirectoryIfMissing True ((intercalate "/" . init . splitOn "/") moduleLoc)
            Prelude.writeFile (moduleLoc Prelude.<> ".ast.show.json")
                    (unpack $ decodeUtf8 $ toStrict $ encodePretty $ Map.fromList (concatMap bindToJSON binds))
            print ("start generating coreAST for module: " <> moduleN <> " at path: " <> moduleLoc,length binds)
            t1 <- getCurrentTime
            l <- mapM (toLBind moduleLoc moduleN) binds
            processedBinds <- evaluate $ force $ map toJSON (catMaybes l)
            t2 <- getCurrentTime
            print $ diffUTCTime t2 t1
            !_  <- DBS.writeFile (moduleLoc Prelude.<> ".lbind.ast.show.json") (toStrict $ encodePretty processedBinds)
            print ("generated coreAST for module: " <> moduleN <> " at path: " <> moduleLoc,length binds)
    return guts

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs

typeOfNumber :: LitNumType -> String
typeOfNumber LitNumInteger = "LitNumInteger"
typeOfNumber LitNumNatural = "LitNumNatural"
typeOfNumber LitNumInt     = "LitNumInt"
typeOfNumber LitNumInt64   = "LitNumInt64"
typeOfNumber LitNumWord    = "LitNumWord"
typeOfNumber LitNumWord64  = "LitNumWord64"

mkLLit :: Literal -> LExpr
mkLLit (LitChar   char) = LLit "LitChar" (pack [char]) False
mkLLit (LitNumber litNumType val _) = LLit (pack $ typeOfNumber litNumType)  (pack $ show val) False
mkLLit (LitString  bs) = LLit "LitString" (pack $ show bs) False
mkLLit LitNullAddr = LLit "LitNullAddr" "" False
mkLLit LitRubbish = LLit "LitRubbish" "" False
mkLLit (LitFloat   rational) = LLit "LitFloat" (pack $ show rational) False
mkLLit (LitDouble  rational) = LLit "LitDouble" (pack $ show rational) False
mkLLit (LitLabel   fs _ IsData) = LLit "LitLabel" (pack $ unpackFS fs) False
mkLLit (LitLabel   fs _ IsFunction) = LLit "LitLabel" (pack $ unpackFS fs) True

mkLVar x = LVar (pack $ nameStableString $ idName x) (pack $ showSDocUnsafe $ ppr $ tyVarKind x) (pack $ showSDocUnsafe $ ppr $ getSrcSpan $ getName x) (isLocalId x) (isExportedId x)

toLexpr :: String -> String ->  Expr Var -> IO LExpr
toLexpr mPath mName (Var x)         = let a = mkLVar x in pure a
toLexpr mPath mName (Lit x)       =  pure $ mkLLit x
toLexpr mPath mName (Type id)       = pure $ LType (pack $ showSDocUnsafe $ ppr id)
toLexpr mPath mName (App func@(App (App (App _ _) pureReturn) (App (App (App (Var x) (Type returnType)) _) condition@(Var cFunInput))) action) =
    case nameStableString (idName x) of
        "$base$Control.Monad$unless" -> toLexpr mPath mName $ Case
                condition
                cFunInput
                returnType
                    [
                        (DEFAULT, [], pureReturn)
                        ,(LitAlt (mkLitString "False"), [], action)
                    ]
        "$base$GHC.Base$when" -> toLexpr mPath mName $ Case
                condition
                cFunInput
                returnType
                    [
                        (DEFAULT, [], pureReturn)
                        ,(LitAlt (mkLitString "True"), [], action)
                    ]
        _ -> do
            f <- toLexpr mPath mName func
            a <- toLexpr mPath mName action
            pure $ LApp f a
toLexpr mPath mName (App func@(App (App (App (App (App (Var x) (Type a)) (Type returnType)) _) leftCase) rightCase) condition@(Var cFunInput)) = do
    let inputVar = mkLocalVar coVarDetails (mkInternalName (mkUnique 'y' 0) (mkVarOcc "y") noSrcSpan) a noCafIdInfo
    if nameStableString (idName x) == "$base$Data.Either$either"
        then toLexpr mPath mName $ Case
                condition
                cFunInput
                returnType
                    [
                        (LitAlt (mkLitString "Left"), [inputVar], App leftCase (Var inputVar))
                        ,(LitAlt (mkLitString "Right"), [inputVar], App rightCase (Var inputVar))
                    ]
        else do
            f <- toLexpr mPath mName func
            _a <- toLexpr mPath mName condition
            pure $ LApp f _a
toLexpr mPath mName (App func@(App (App (App (App (Var x) (Type returnType)) (Type a)) defaultCase) justCase) condition@(Var conditionInput)) = do
    let inputVar = mkLocalVar coVarDetails (mkInternalName (mkUnique 'y' 0) (mkVarOcc "y") noSrcSpan) a noCafIdInfo
    if nameStableString (idName x) == "$base$Data.Maybe$maybe"
        then toLexpr mPath mName $ Case
                condition
                conditionInput
                returnType
                    [
                        (LitAlt (mkLitString "Nothing"), [], defaultCase)
                        ,(LitAlt (mkLitString "Just"), [inputVar], App justCase (Var inputVar))
                    ]
        else do
            f <- toLexpr mPath mName func
            _a <- toLexpr mPath mName condition
            pure $ LApp f _a
toLexpr mPath mName (App func@(App (App (App _ _) pureReturn) (App (App (App (Var x) (Type returnType)) _) condition@(App _ (Var cFunInput)))) action) =
    case nameStableString (idName x) of
        "$base$Control.Monad$unless" -> toLexpr mPath mName $ Case
                condition
                cFunInput
                returnType
                    [
                        (DEFAULT, [], pureReturn)
                        ,(LitAlt (mkLitString "False"), [], action)
                    ]
        "$base$GHC.Base$when" -> toLexpr mPath mName $ Case
                condition
                cFunInput
                returnType
                    [
                        (DEFAULT, [], pureReturn)
                        ,(LitAlt (mkLitString "True"), [], action)
                    ]
        _ -> do
            f <- toLexpr mPath mName func
            a <- toLexpr mPath mName action
            pure $ LApp f a
toLexpr mPath mName (App func@(App (App (App (App (App (Var x) (Type a)) (Type returnType)) _) leftCase) rightCase) condition@(App _ (Var cFunInput))) = do
    let inputVar = mkLocalVar coVarDetails (mkInternalName (mkUnique 'y' 0) (mkVarOcc "y") noSrcSpan) a noCafIdInfo
    if nameStableString (idName x) == "$base$Data.Either$either"
        then toLexpr mPath mName $ Case
                condition
                cFunInput
                returnType
                    [
                        (LitAlt (mkLitString "Left"), [inputVar], App leftCase (Var inputVar))
                        ,(LitAlt (mkLitString "Right"), [inputVar], App rightCase (Var inputVar))
                    ]
        else do
            f <- toLexpr mPath mName func
            _a <- toLexpr mPath mName condition
            pure $ LApp f _a
toLexpr mPath mName (App func@(App (App (App (App (Var x) (Type returnType)) (Type a)) defaultCase) justCase) condition@(App _ (Var conditionInput))) = do
    let inputVar = mkLocalVar coVarDetails (mkInternalName (mkUnique 'y' 0) (mkVarOcc "y") noSrcSpan) a noCafIdInfo
    if nameStableString (idName x) == "$base$Data.Maybe$maybe"
        then toLexpr mPath mName $ Case
                condition
                conditionInput
                returnType
                    [
                        (LitAlt (mkLitString "Nothing"), [], defaultCase)
                        ,(LitAlt (mkLitString "Just"), [inputVar], App justCase (Var inputVar))
                    ]
        else do
            f <- toLexpr mPath mName func
            _a <- toLexpr mPath mName condition
            pure $ LApp f _a
toLexpr mPath mName (App func args) = do
    f <- toLexpr mPath mName func
    a <- toLexpr mPath mName args
    pure $ LApp f a
toLexpr mPath mName (Lam func args) = do
    a <- toLexpr mPath mName args
    pure $ LLam (pack $ nameStableString (idName func)) a
toLexpr mPath mName (Let func args) = do
    a <- toLexpr mPath mName args
    f <- toLBind' mPath mName func
    pure $ LLet f a
toLexpr mPath mName (Case condition bind _type alts) = do
    n <- modifyMVar caseCounter (\x -> pure (x + 1, x))
    c <- toLexpr mPath mName condition
    caseExprHashable <- processCaseExprToHash condition
    print caseExprHashable
    a <- mapM (toLAlt mPath mName) alts
    DBS.appendFile (mPath <> ".case") (toStrict $ encode (pack $ replace "\t" "" $ replace "\n" "" $ showSDocUnsafe $ ppr condition) <> "\t" <> encode (show c) <> "\n")
    pure $ LCase (pack $ mName <> show n) caseExprHashable c (pack $ replace "\n" "" $ showSDocUnsafe $ ppr condition) (pack $ nameStableString (idName bind)) (pack $ showSDocUnsafe $ ppr _type) a
toLexpr _ _ v = pure $ LUnhandled (pack $ show $ toConstr v) (pack $ showSDocUnsafe $ ppr v)

processCaseExprToHash :: Expr Var -> IO (Maybe CaseExtract)
processCaseExprToHash (App (Var appfunctionName) (Var inputField))
    = pure $ Just (AppOnVar (pack $ nameStableString $ idName appfunctionName) (pack $ nameStableString $ idName inputField) (pack $ showSDocUnsafe $ ppr $ tyVarKind inputField))
processCaseExprToHash (App (App (Var appfunctionName) (Type appfunctionOutputType)) getfield@(App (App (App (App (App (App (Var _) (Type _)) (Type _)) (Type _)) (Type _)) (Var _)) (Var _)))
    = do
        getField <- processCaseExprToHash getfield
        pure $ (Just . AppOnGetField (pack $ nameStableString $ idName appfunctionName) (pack $ showSDocUnsafe $ ppr appfunctionOutputType)) =<< getField
processCaseExprToHash (App (App (App (Var appfunctionName) (Type functionReturnType)) (Var defaultValue)) getfield@(App (App (App (App (App (App (Var _) (Type _)) (Type _)) (Type _)) (Type _)) (Var _)) (Var _)))
    = do
        getField <- processCaseExprToHash getfield
        pure $ (Just . MaybeOrEitherOnGetField (pack $ nameStableString $ idName appfunctionName) (pack $ showSDocUnsafe $ ppr functionReturnType) (pack $ nameStableString $ idName defaultValue)) =<< getField
processCaseExprToHash (App (App (App (App (App (App (Var functionName) (Type _)) (Type fieldName)) (Type objType)) (Type fieldType)) (Var _)) (Var _))
    = pure $ Just $ GetField (pack $ nameStableString $ idName functionName) (pack $ showSDocUnsafe $ ppr fieldName) (pack $ showSDocUnsafe $ ppr objType) (pack $ showSDocUnsafe $ ppr fieldType) -- (pack $ nameStableString $ idName hasField) (pack $ nameStableString $ idName field)
processCaseExprToHash condition@(Var field) = pure $ Just $ OnField (pack $ nameStableString $ idName field) (pack $ showSDocUnsafe $ ppr $ tyVarKind field) (pack $ replace "\t" "" $ replace "\n" "" $ showSDocUnsafe $ ppr condition)
processCaseExprToHash ((App (App (Var functionName) (Type functionOutputType)) (Var input))) = pure $ Just $ AppOnField (pack $ nameStableString $ idName functionName) (pack $ replace "\t" "" $ replace "\n" "" $ showSDocUnsafe $ ppr functionOutputType) (pack $ nameStableString $ idName input) (pack $ showSDocUnsafe $ ppr $ tyVarKind input) --(pack $ replace "\t" "" $ replace "\n" "" $ showSDocUnsafe $ ppr condition)
processCaseExprToHash ((App (App (App (Var functionName) (Type inputType)) (App (Var subFunction) (Type subFunctionInputType))) (Var input))) = pure $ Just $ AppAppOnField (pack $ nameStableString $ idName functionName) (pack $ replace "\t" "" $ replace "\n" "" $ showSDocUnsafe $ ppr inputType) (pack $ nameStableString $ idName subFunction) (pack $ replace "\t" "" $ replace "\n" "" $ showSDocUnsafe $ ppr subFunctionInputType) (pack $ nameStableString $ idName input) (pack $ showSDocUnsafe $ ppr $ tyVarKind input) --(pack $ replace "\t" "" $ replace "\n" "" $ showSDocUnsafe $ ppr condition)
processCaseExprToHash _ = pure Nothing

caseCounter :: MVar Int64
{-# NOINLINE caseCounter #-}
caseCounter = unsafePerformIO (newMVar 0)

toLAlt :: String -> String -> (AltCon, [Var], CoreExpr) -> IO (LAltCon, [LExpr], LExpr)
toLAlt mPath mName (DataAlt dataCon, val, e) = do
    a <- toLexpr mPath mName e
    pure (LDataAlt (pack $ showSDocUnsafe $ ppr dataCon), map mkLVar val,a)
toLAlt mPath mName (LitAlt lit, val, e) = do
    a <- toLexpr mPath mName e
    pure (LLitAlt (pack $ showSDocUnsafe $ ppr lit), map mkLVar val,a)
toLAlt mPath mName (DEFAULT, val, e) = do
    a <- toLexpr mPath mName e
    pure (LDEFAULT, map mkLVar val, a)

shouldFilter :: Text -> Bool
shouldFilter x = "$_in$$" `isInfixOf` x || "$_sys$" `isInfixOf` x || "$$" `isInfixOf` x

toLBind' :: String -> String ->  CoreBind -> IO LBind
toLBind' mPath mName (NonRec binder expr) = do
    LNonRec (pack $ nameStableString $ idName binder) (pack $ showSDocUnsafe $ ppr $ tyVarKind binder) <$> toLexpr mPath mName expr
toLBind' mPath mName (Rec binds) = do
    r <- mapM (\(b, e) ->
        if shouldFilter (pack $ nameStableString $ idName b)
            then pure Nothing
            else do
                a <- toLexpr mPath mName e
                pure $ Just (pack $ nameStableString (idName b),pack $ showSDocUnsafe $ ppr $ tyVarKind b, a)
            )
        binds
    pure $ LRec $ catMaybes r

toLBind :: String -> String -> CoreBind -> IO (Maybe LBind)
toLBind mPath mName (NonRec binder expr) = do
    if shouldFilter (pack $ nameStableString $ idName binder)
        then pure Nothing
        else do
            a <- toLexpr mPath mName expr
            pure $ Just $ LNonRec (pack $ nameStableString $ idName binder) (pack $ showSDocUnsafe $ ppr $ tyVarKind binder) a
toLBind mPath mName (Rec binds) = do
    r <- mapM (\(b, e) ->
            if shouldFilter (pack $ nameStableString $ idName b)
                then pure Nothing
                else do
                    a <- toLexpr mPath mName e
                    pure $ Just (pack $ nameStableString (idName b),pack $ showSDocUnsafe $ ppr $ tyVarKind b,a)
                ) binds
    pure $ Just $ LRec $ catMaybes r