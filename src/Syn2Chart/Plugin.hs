{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass,DeriveDataTypeable #-}

module Syn2Chart.Plugin (plugin) where

import Control.Concurrent
import CoreMonad
import GhcPlugins
import qualified Data.Functor
import Data.List.Extra (nub)
import Data.Data (toConstr)
import Data.Aeson
import Data.Maybe
import CoreSyn
import qualified Data.HashMap.Strict as HM
import Var
import GHC.Generics (Generic)
import Module
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Data.Data

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
        print ("generating coresyn json for: " Prelude.<> moduleN)
        Data.ByteString.Lazy.writeFile (moduleLoc Prelude.<> ".syn.json") $ encodePretty res
    return guts

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs

translateCoreProgramToCFG :: [Bind CoreBndr] -> IO [Node]
translateCoreProgramToCFG r = do
    res <- mapM translateCoreBindToCFG r
    pure $ concat res

translateCoreBindToCFG :: Bind CoreBndr -> IO [Node]
translateCoreBindToCFG (NonRec binds expr) = do
    res <- buildGraph' (Just (nameStableString $ idName binds)) expr
    pure [res]
translateCoreBindToCFG (Rec bindings) =
    mapM (\(name ,body) -> do
        res <- buildGraph' (Just $ nameStableString $ idName name) body
        pure $ res
    ) bindings

showP :: (Outputable b) => b -> String
showP = showSDocUnsafe . ppr

data CaseAlts = CaseAlts String String Node
    deriving (Generic,Eq,Data,Show)

data Node = LAM String Node | FUNCTION Node Node | CASE Node String [CaseAlts] | END String String | VAR String | LIT String String
    deriving (Generic,Eq,Data,Show)

instance ToJSON Node where
    toJSON (LAM str x) = Object $ HM.fromList ([("constr", (String "lambda")),("name", toJSON str),("body",toJSON x)])
    toJSON (LAM str x) = Object $ HM.fromList ([("constr", (String "lambda")),("name", toJSON str),("body",toJSON x)])
    toJSON (FUNCTION node n) = Object $ HM.fromList ([("constr", String "function"),("name", toJSON node),("args",toJSON n)])
    toJSON (CASE condition _type choices) = Object $ HM.fromList ([("constr", String "case"),("condition", toJSON condition),("_type",toJSON _type),("choices",toJSON choices)])
    toJSON (VAR name) = toJSON name
    toJSON (LIT name _type) = Object $ HM.fromList ([("name",toJSON name ),("type", toJSON _type)])
    toJSON (END str body) = Object $ HM.fromList ([("type", String "END"),("constr", toJSON str),("body", toJSON body)])

instance ToJSON CaseAlts where
    toJSON (CaseAlts val _type exprs) = Object $ HM.fromList ([("value", toJSON val),("name", toJSON _type),("body",toJSON exprs)])

buildGraph' :: Maybe String -> Expr CoreBndr -> IO Node
buildGraph' fName expr =
    case expr of
        App fun args -> do
            let functionName = showP fun
            __args <- buildGraph' Nothing args
            f' <- case (show $ toConstr fun,fun) of
                        ("Var",Var i) -> pure $ VAR $ nameStableString $ idName i
                        ("Lit",Lit i) -> pure $ LIT (showP i) (showP $ literalType i)
                        _ -> buildGraph' Nothing args
            pure (FUNCTION (maybe f' VAR (fName)) __args)
        Lam b e -> do
            let functionName = showP b
            _args <- buildGraph' Nothing e
            pure $ LAM (fromMaybe functionName fName) _args
        Let b e -> do
            let functionName = showP b
            _args <- buildGraph' Nothing e
            pure $ LAM (fromMaybe functionName fName) _args
        Case e b t alts -> do
            condition <- buildGraph' Nothing e
            choices <- mapM (\(altcon,b,expr)-> do
                expr' <- buildGraph' Nothing expr
                pure $ CaseAlts (showP altcon) (showP b) (expr')
                ) alts
            pure $ CASE condition (showP t) choices
        Var i -> do
            print (showP $ idType i,idUnique i)
            pure $ VAR $ nameStableString $ idName i
        Lit i -> pure $ LIT (showP i) (showP $ literalType i)
        a -> pure $ END (show $ toConstr a) (showP a)