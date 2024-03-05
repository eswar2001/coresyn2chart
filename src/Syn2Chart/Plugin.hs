{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}

module Syn2Chart.Plugin (plugin) where

import Control.Monad
import CoreMonad
import CoreSyn hiding (TB)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile, fromStrict, toStrict)
import Data.Data
import Data.Maybe
import GhcPlugins hiding (TB,(<>))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Bool
import Data.Text (pack)
import Prelude hiding (id)
import Control.Concurrent (forkIO)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import qualified Data.ByteString.Base64 as BS
import System.IO 
import System.IO.Extra (openFile)


plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
    , pluginRecompile = GhcPlugins.purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = return (CoreDoPluginPass "CoreSyn2Chart" buildCfgPass  : todos)

buildCfgPass :: ModGuts -> CoreM ModGuts
buildCfgPass guts = do
    _ <- liftIO $ do
        let binds = mg_binds guts
            moduleN = moduleNameString $ moduleName $ mg_module guts
            moduleLoc = getFilePath $ mg_loc guts
        res <- translateCoreProgramToCFG binds
        let l = map (\t@(Function name _type _ _) -> (name,t)) res
        foldM (\acc (functionName,functionbody) -> do
                        val <- fileWrite (moduleLoc Prelude.<> (unpack $ decodeUtf8 $ BS.encode $ toStrict $ encode functionName) Prelude.<> ".syn.json") (unpack $ decodeUtf8 $ toStrict $ encodePretty functionbody)
                        pure ([val] ++ acc)
                      ) [] l
        !_ <- Prelude.writeFile (moduleLoc Prelude.<> ".ast.show.json")
                (unpack $ decodeUtf8 $ toStrict $ encode $ Map.fromList (concatMap bindToJSON binds))
        pure ()
    return guts

fileWrite :: String -> String -> IO ()
fileWrite fileName content = do
  handle <- openFile fileName WriteMode
  hPutStr handle content
  hClose handle

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs

data Function = Function String String Bool [Function]
    deriving (Show)

instance ToJSON Function where
    toJSON (Function name _type isCase f') =
        Object $
            HM.fromList [("name",toJSON name),("body",toJSON f'),("type",toJSON _type),("isCase",toJSON isCase)]

instance ToJSON Var where
  toJSON var = String $ pack $ nameStableString (idName var)

instance ToJSON (Expr Var) where
  toJSON (Var id) = toJSON id
  toJSON (Type id) = String $ pack $ showSDocUnsafe $ ppr id
  toJSON x@(Lit _) =
    object
      [ "type" .= String "Lit",
        "literal" .= pack (showSDocUnsafe (ppr x))
      ]
  toJSON (App (Var id) (Type t)) =
    object
      [ "type" .= String "App",
        "function" .= toJSON id,
        "argument" .= String (pack $ showSDocUnsafe $ ppr t)
      ]
  toJSON (App fun (Type _)) = toJSON fun
  toJSON (App fun arg) =
    object
      [ "type" .= String "App",
        "function" .= toJSON fun,
        "argument" .= toJSON arg
      ]
  toJSON (Lam bind expr) =
    object
      [ "type" .= String "Lam",
        "function" .= toJSON bind,
        "argument" .= toJSON expr
      ]
  toJSON (Let bind expr) =
    object
      [ "type" .= String "Let",
        "function_list" .=
            let bindsList = bindToJSON bind
            in Map.fromList bindsList,
        "argument" .= toJSON expr
      ]
  toJSON (Case condition bind _type alts) =
    object
      [ "type" .= String "Case",
        "condition_type" .= String (pack $ showSDocUnsafe $ ppr _type),
        "condition" .= toJSON condition,
        "function" .= toJSON bind,
        "matches" .= toJSON alts
      ]
  toJSON v = object ["unhandled" .= String (pack $ show $ toConstr v),"value" .= String (pack $ showSDocUnsafe $ ppr v)]

instance ToJSON AltCon where
  toJSON (DataAlt dataCon) = String (pack $ showSDocUnsafe $ ppr dataCon)
  toJSON (LitAlt lit) = String (pack $ showSDocUnsafe $ ppr lit)
  toJSON DEFAULT = String "DEFAULT"

bindToJSON :: CoreBind -> [(String, Value)]
bindToJSON (NonRec binder ((Lam _ expr))) =
  [(nameStableString (idName binder), toJSON expr)]
bindToJSON (NonRec binder expr) =
  [(nameStableString (idName binder), toJSON expr)]
bindToJSON (Rec binds) =
  map (\(b, e) -> (nameStableString (idName b), toJSON e)) binds

translateCoreProgramToCFG :: [Bind CoreBndr] -> IO [Function]
translateCoreProgramToCFG r =
    pure $ concatMap countBindings r

getPivotName :: Var -> String
getPivotName var = nameStableString $ idName var

traverseForFlows :: CoreExpr -> [Function] -> [Function]
traverseForFlows (Var x) argument =
    let name = getPivotName x
    in [Function name (showSDocUnsafe $ ppr $ tyVarKind x) False argument]
traverseForFlows (Lit _) argument = argument
traverseForFlows (Type _) argument = argument
traverseForFlows (Coercion _) argument = argument
traverseForFlows (App (Var id) (Lit x)) argument = [Function (getPivotName id) "" False [Function (showSDocUnsafe $ ppr x) "" False argument]]
traverseForFlows (App (Var id) (Var x)) argument = [Function (getPivotName id) "" False [Function (getPivotName x) "" False argument]]
traverseForFlows (App (Var id) (Type _)) argument = [Function (getPivotName id) "" False argument]
traverseForFlows (App f a) argument =
    let arg = traverseForFlows a argument
        fun = traverseForFlows f arg
   in fun
traverseForFlows (Lam e a) argument = [Function (getPivotName e) "" False (traverseForFlows a argument)]
traverseForFlows (Let b e) argument = 
  let arg = traverseForFlows e argument
  in countBindingsInternal b arg
traverseForFlows (Case e x t alts) argument = concatMap (\x -> countAlt t x argument) alts
traverseForFlows (Cast e _) argument = traverseForFlows e argument
traverseForFlows (Tick _ e) argument = traverseForFlows e argument

countBindings :: CoreBind -> [Function]
countBindings (NonRec binds expr) =
  let maybeName = Just (nameStableString $ idName binds)
  in maybe [] (\name -> [Function name (showSDocUnsafe $ ppr $ tyVarKind binds) False $ traverseForFlows expr []]) maybeName
countBindings (Rec bs) =
  mapMaybe (\(binds,expr) ->
    let maybeName = Just (nameStableString $ idName binds)
    in (\name -> Just $ Function name (showSDocUnsafe $ ppr $ tyVarKind binds) False $ traverseForFlows expr []) =<< maybeName) bs

countBindingsInternal :: CoreBind -> [Function] -> [Function]
countBindingsInternal (NonRec _ expr) argument = traverseForFlows expr argument
countBindingsInternal (Rec bs) argument = concatMap ((`traverseForFlows` argument) . snd) bs

countAlt :: Type -> (AltCon, [Var], CoreExpr) -> [Function] -> [Function]
countAlt t (p, [], e) args = [Function (showSDocUnsafe $ ppr p) (showSDocUnsafe $ ppr t) True $ traverseForFlows e args]
countAlt t (p, val, e) args = [Function (showSDocUnsafe $ ppr p) (showSDocUnsafe $ ppr t) True $ traverseForFlows e args]

