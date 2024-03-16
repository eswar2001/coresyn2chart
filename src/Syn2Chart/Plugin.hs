{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Syn2Chart.Plugin (plugin) where

import Syn2Chart.Types ( LBind(LRec, LNonRec), bindToJSON, LAltCon(..), LExpr(..) )
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
      noSrcSpan,
      mkLocalVar,
      tyVarKind,
      ModGuts(mg_binds, mg_module, mg_loc),
      Module(moduleName),
      Outputable(ppr),
      CommandLineOption,
      RealSrcSpan(srcSpanFile),
      SrcSpan(..), Var )
import qualified Data.Map as Map
import Prelude hiding (id)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import Data.List.Extra (replace,intercalate,splitOn)
import System.Directory (createDirectoryIfMissing)
import Unique ( mkUnique )

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
        createDirectoryIfMissing True ((intercalate "/" . init . reverse . splitOn "/") moduleLoc)
        !_ <- Prelude.writeFile (moduleLoc Prelude.<> ".ast.show.json")
                (unpack $ decodeUtf8 $ toStrict $ encodePretty $ Map.fromList (concatMap bindToJSON binds))
        DBS.writeFile (moduleLoc Prelude.<> ".lbind.ast.show.json") (toStrict $ encodePretty $ map (toJSON . toLBind) binds)
        print ("generated coreAST for module: " <> moduleN <> " at path: " <> moduleLoc)
        pure ()
    return guts

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs

toLexpr :: Expr Var -> LExpr
toLexpr (Var x)         = LVar (nameStableString $ idName x) (showSDocUnsafe $ ppr $ tyVarKind x)
toLexpr x@(Lit _)       = LLit (showSDocUnsafe $ ppr x)
toLexpr (Type id)       = LType (showSDocUnsafe $ ppr id)
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
toLexpr (Lam func args) = LLam (nameStableString (idName func)) (toLexpr args)
toLexpr (Let func args) = LLet (toLBind func) (toLexpr args)
toLexpr (Case condition bind _type alts) = LCase (toLexpr condition) (replace "\n" "" $ showSDocUnsafe $ ppr condition) (nameStableString (idName bind)) (showSDocUnsafe $ ppr _type) (map toLAlt alts)
toLexpr v = LUnhandled (show $ toConstr v) (showSDocUnsafe $ ppr v)

toLAlt :: (AltCon, [Var], CoreExpr) -> (LAltCon, [LExpr], LExpr)
toLAlt (DataAlt dataCon, val, e) = (LDataAlt (showSDocUnsafe $ ppr dataCon), map (\x -> LVar (nameStableString $ idName x) (showSDocUnsafe $ ppr $ tyVarKind x)) val, toLexpr e)
toLAlt (LitAlt lit, val, e) = (LLitAlt (showSDocUnsafe $ ppr lit), map (\x -> LVar (nameStableString $ idName x) (showSDocUnsafe $ ppr $ tyVarKind x)) val, toLexpr e)
toLAlt (DEFAULT, val, e) = (LDEFAULT, map (\x -> LVar (nameStableString $ idName x) (showSDocUnsafe $ ppr $ tyVarKind x)) val, toLexpr e)

toLBind :: CoreBind -> LBind
toLBind (NonRec binder expr) = LNonRec (nameStableString $ idName binder) (showSDocUnsafe $ ppr $ tyVarKind binder) (toLexpr expr)
toLBind (Rec binds) = LRec (map (\(b, e) -> (nameStableString (idName b),showSDocUnsafe $ ppr $ tyVarKind b, toLexpr e)) binds)