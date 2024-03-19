{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use init" #-}

module Syn2Chart.Plugin (plugin) where

import Syn2Chart.Types ( LBind(LRec, LNonRec), LAltCon(..), LExpr(..), bindToJSON )
import CoreMonad ( liftIO, CoreM, CoreToDo(CoreDoPluginPass) )
import CoreSyn
    ( AltCon(..),
      Expr(Case, Lit, Type, Var, App, Lam, Let),
      CoreBind,
      Bind(Rec, NonRec),
      CoreExpr )
import Data.Aeson ( ToJSON(toJSON) )
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
      CommandLineOption, Var, NamedThing (getName), Literal (..), FunctionOrData (..), LitNumType (..) )
import Prelude hiding (id)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack, Text)
import Data.List.Extra (replace,intercalate,splitOn,isSuffixOf)
import Data.Maybe (mapMaybe)
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
import Data.Text (pack,isInfixOf)

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
        -- !_ <- Prelude.writeFile (moduleLoc Prelude.<> ".ast.show.json")
        --             (unpack $ decodeUtf8 $ toStrict $ encodePretty $ Map.fromList (concatMap bindToJSON binds))
        unless ("Types.hs" `isSuffixOf` moduleN || "EC." `isInfixOf` pack moduleN) $ do
            createDirectoryIfMissing True ((intercalate "/" . reverse . tail . reverse . splitOn "/") moduleLoc)
            print ("start generating coreAST for module: " <> moduleN <> " at path: " <> moduleLoc,length binds)
            t1 <- getCurrentTime
            processedBinds <- evaluate $ force $ map toJSON (mapMaybe toLBind binds)
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

toLexpr :: Expr Var -> LExpr
toLexpr (Var x)         = mkLVar x
toLexpr (Lit x)       =  mkLLit x
toLexpr (Type id)       = LType (pack $ showSDocUnsafe $ ppr id)
toLexpr (App func@(App (App (App _ _) pureReturn) (App (App (App (Var x) (Type returnType)) _) condition@(Var cFunInput))) action) =
    case nameStableString (idName x) of
        "$base$Control.Monad$unless" -> toLexpr $ Case
                condition
                cFunInput
                returnType
                    [
                        (DEFAULT, [], pureReturn)
                        ,(LitAlt (mkLitString "False"), [], action)
                    ]
        "$base$GHC.Base$when" -> toLexpr $ Case
                condition
                cFunInput
                returnType
                    [
                        (DEFAULT, [], pureReturn)
                        ,(LitAlt (mkLitString "True"), [], action)
                    ]
        _ -> LApp (toLexpr func) (toLexpr action)
toLexpr (App func@(App (App (App (App (App (Var x) (Type a)) (Type returnType)) _) leftCase) rightCase) condition@(Var cFunInput)) = do
    let inputVar = mkLocalVar coVarDetails (mkInternalName (mkUnique 'y' 0) (mkVarOcc "y") noSrcSpan) a noCafIdInfo
    if nameStableString (idName x) == "$base$Data.Either$either"
        then toLexpr $ Case
                condition
                cFunInput
                returnType
                    [
                        (LitAlt (mkLitString "Left"), [inputVar], App leftCase (Var inputVar))
                        ,(LitAlt (mkLitString "Right"), [inputVar], App rightCase (Var inputVar))
                    ]
        else LApp (toLexpr func) (toLexpr condition)
toLexpr (App func@(App (App (App (App (Var x) (Type returnType)) (Type a)) defaultCase) justCase) condition@(Var conditionInput)) = do
    let inputVar = mkLocalVar coVarDetails (mkInternalName (mkUnique 'y' 0) (mkVarOcc "y") noSrcSpan) a noCafIdInfo
    if nameStableString (idName x) == "$base$Data.Maybe$maybe"
        then toLexpr $ Case
                condition
                conditionInput
                returnType
                    [
                        (LitAlt (mkLitString "Nothing"), [], defaultCase)
                        ,(LitAlt (mkLitString "Just"), [inputVar], App justCase (Var inputVar))
                    ]
        else LApp (toLexpr func) (toLexpr condition)
toLexpr (App func@(App (App (App _ _) pureReturn) (App (App (App (Var x) (Type returnType)) _) condition@(App _ (Var cFunInput)))) action) =
    case nameStableString (idName x) of
        "$base$Control.Monad$unless" -> toLexpr $ Case
                condition
                cFunInput
                returnType
                    [
                        (DEFAULT, [], pureReturn)
                        ,(LitAlt (mkLitString "False"), [], action)
                    ]
        "$base$GHC.Base$when" -> toLexpr $ Case
                condition
                cFunInput
                returnType
                    [
                        (DEFAULT, [], pureReturn)
                        ,(LitAlt (mkLitString "True"), [], action)
                    ]
        _ -> LApp (toLexpr func) (toLexpr action)
toLexpr (App func@(App (App (App (App (App (Var x) (Type a)) (Type returnType)) _) leftCase) rightCase) condition@(App _ (Var cFunInput))) = do
    let inputVar = mkLocalVar coVarDetails (mkInternalName (mkUnique 'y' 0) (mkVarOcc "y") noSrcSpan) a noCafIdInfo
    if nameStableString (idName x) == "$base$Data.Either$either"
        then toLexpr $ Case
                condition
                cFunInput
                returnType
                    [
                        (LitAlt (mkLitString "Left"), [inputVar], App leftCase (Var inputVar))
                        ,(LitAlt (mkLitString "Right"), [inputVar], App rightCase (Var inputVar))
                    ]
        else LApp (toLexpr func) (toLexpr condition)
toLexpr (App func@(App (App (App (App (Var x) (Type returnType)) (Type a)) defaultCase) justCase) condition@(App _ (Var conditionInput))) = do
    let inputVar = mkLocalVar coVarDetails (mkInternalName (mkUnique 'y' 0) (mkVarOcc "y") noSrcSpan) a noCafIdInfo
    if nameStableString (idName x) == "$base$Data.Maybe$maybe"
        then toLexpr $ Case
                condition
                conditionInput
                returnType
                    [
                        (LitAlt (mkLitString "Nothing"), [], defaultCase)
                        ,(LitAlt (mkLitString "Just"), [inputVar], App justCase (Var inputVar))
                    ]
        else LApp (toLexpr func) (toLexpr condition)
toLexpr (App func args) = LApp (toLexpr func) (toLexpr args)
toLexpr (Lam func args) = LLam (pack $ nameStableString (idName func)) (toLexpr args)
toLexpr (Let func args) = LLet (toLBind' func) (toLexpr args)
toLexpr (Case condition bind _type alts) = LCase (toLexpr condition) (pack $ replace "\n" "" $ showSDocUnsafe $ ppr condition) (pack $ nameStableString (idName bind)) (pack $ showSDocUnsafe $ ppr _type) (map toLAlt alts)
toLexpr v = LUnhandled (pack $ show $ toConstr v) (pack $ showSDocUnsafe $ ppr v)

toLAlt :: (AltCon, [Var], CoreExpr) -> (LAltCon, [LExpr], LExpr)
toLAlt (DataAlt dataCon, val, e) = (LDataAlt (pack $ showSDocUnsafe $ ppr dataCon), map mkLVar val, toLexpr e)
toLAlt (LitAlt lit, val, e) = (LLitAlt (pack $ showSDocUnsafe $ ppr lit), map mkLVar val, toLexpr e)
toLAlt (DEFAULT, val, e) = (LDEFAULT, map mkLVar val, toLexpr e)

shouldFilter :: Text -> Bool
shouldFilter x = "$_in$$" `isInfixOf` x || "$_sys$" `isInfixOf` x || "$$" `isInfixOf` x

toLBind' :: CoreBind -> LBind
toLBind' (NonRec binder expr) = LNonRec (pack $ nameStableString $ idName binder) (pack $ showSDocUnsafe $ ppr $ tyVarKind binder) (toLexpr expr)
toLBind' (Rec binds) = LRec (mapMaybe (\(b, e) -> if shouldFilter (pack $ nameStableString $ idName b) then Nothing else Just (pack $ nameStableString (idName b),pack $ showSDocUnsafe $ ppr $ tyVarKind b, toLexpr e)) binds)

toLBind :: CoreBind -> Maybe LBind
toLBind (NonRec binder expr) = if shouldFilter (pack $ nameStableString $ idName binder) then Nothing else Just $ LNonRec (pack $ nameStableString $ idName binder) (pack $ showSDocUnsafe $ ppr $ tyVarKind binder) (toLexpr expr)
toLBind (Rec binds) = Just $ LRec (mapMaybe (\(b, e) -> if shouldFilter (pack $ nameStableString $ idName b) then Nothing else Just (pack $ nameStableString (idName b),pack $ showSDocUnsafe $ ppr $ tyVarKind b, toLexpr e)) binds)