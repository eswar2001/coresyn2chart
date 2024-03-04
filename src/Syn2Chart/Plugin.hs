{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syn2Chart.Plugin (plugin) where

import Control.Monad
import CoreMonad
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Data.Data
import Module
import Outputable
import GhcPlugins hiding (TB,(<>))
import Prelude hiding (id)
import Data.Text (pack)

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
        let res = map bindToJSON binds
        Data.ByteString.Lazy.writeFile (moduleLoc Prelude.<> ".syn.json") (encodePretty res)
        pure ()
    return guts

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs


instance ToJSON Var where
    toJSON var = String $ pack $ nameStableString (idName var)

instance ToJSON (Expr Var) where
    toJSON (Var id) = toJSON id
    toJSON (Type id) = String $ pack $ showSDocUnsafe $ ppr id
    toJSON x@(Lit _) = object [
        "type" .= String "Lit",
        "literal" .= (pack $ showSDocUnsafe (ppr x))
        ]
    toJSON (App (Var id) (Type t)) = toJSON id
    toJSON (App fun (Type t)) = toJSON fun
    toJSON (App fun arg) = object [
        "type" .= String "App",
        "function" .= toJSON fun,
        "argument" .= toJSON arg
        ]
    toJSON (Lam bind expr) = object [
        "type" .= String "Lam",
        "function" .= toJSON bind,
        "argument" .= toJSON expr
        ]
    toJSON (Let bind expr) = object [
        "type" .= String "Let",
        "function" .= bindToJSON bind,
        "argument" .= toJSON expr
        ]
    toJSON (Case condition bind _type alts) = object [
        "type" .= String "Case",
        "condition_type" .= String (pack $ showSDocUnsafe $ ppr _type),
        "condition" .= toJSON condition,
        "function" .= toJSON bind,
        "matches" .= toJSON alts
        ]
    toJSON v = object ["unhandled" .= String (pack $ show $ toConstr v)]

instance ToJSON AltCon where
    toJSON (DataAlt dataCon) = String (pack $ showSDocUnsafe $ ppr dataCon)
    toJSON (LitAlt  lit) = String (pack $ showSDocUnsafe $ ppr lit)
    toJSON DEFAULT = String "DEFAULT"

bindToJSON :: CoreBind -> Value
bindToJSON (NonRec binder ((Lam bind expr))) = object [
    "binder" .= nameStableString (idName binder),
    "expr" .= toJSON expr
    ]
bindToJSON (NonRec binder expr) = object [
    "binder" .= nameStableString (idName binder),
    "expr" .= toJSON expr
    ]
bindToJSON (Rec binds) = object [
    "rec_binds" .= map (\(b, e) -> object [
        "binder" .= nameStableString (idName b),
        "expr" .= toJSON e]) binds
    ]
