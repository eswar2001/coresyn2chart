{-# LANGUAGE FlexibleContexts,BangPatterns,FlexibleInstances,UndecidableInstances #-}

module Syn2Chart.Plugin (plugin) where

import System.Directory
import System.FilePath
import Control.Concurrent
import Control.Monad
import CoreMonad
import CoreStats
import CoreSyn hiding (TB)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (writeFile)
import Data.Data
import Data.List.Extra (nub)
import Data.Maybe
import Data.Tree
import Demand
import GHC.Generics (Generic)
import GhcPlugins hiding (TB,(<>))
import Module
import Outputable
import qualified Data.ByteString.Lazy as BS
import qualified Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as Text
import Var


plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
    , pluginRecompile = GhcPlugins.purePlugin
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = return (CoreDoPluginPass "CoreSyn2Chart" buildCfgPass  : todos)

buildCfgPass :: ModGuts -> CoreM ModGuts
buildCfgPass guts = do
    _ <- liftIO $ forkIO $ do
        let binds = mg_binds guts
            moduleN = moduleNameString $ moduleName $ mg_module guts
            moduleLoc = getFilePath $ mg_loc guts
        res <- translateCoreProgramToCFG binds
        _ <- foldM (\acc x@(Function name _) -> do
                _ <- Prelude.writeFile (moduleLoc Prelude.<> "--" Prelude.<> name Prelude.<> ".syn.log") (drawTree $ toDataTree x)
                pure $ (acc + 1)
            ) 0 res
        _ <- Data.ByteString.Lazy.writeFile (moduleLoc Prelude.<> ".syn.json") (encodePretty res)
        pure ()
    return guts

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs

data Function = Function String [Function]
    deriving (Show)

instance ToJSON Function where
    toJSON (Function name f') = Object $ HM.fromList [(Text.pack name,toJSON f')]

instance Semigroup Function where
    (Function name1 children1) <> (Function name2 children2) =
        if name1 == "" || name2 == "" then
            Function (name1 ++ name2) (children1 Prelude.<> children2)
        else
            Function (name1 ++ "__" ++ name2) (children1 Prelude.<> children2)

instance Monoid Function where
    mempty = Function "" []

toDataTree :: Function -> Tree String
toDataTree (Function name children) = Node name (map toDataTree children)

treeToString :: Function -> String
treeToString (Function name children) = name ++ "\n" ++ concatMap (\child -> "  " ++ treeToString child) children

translateCoreProgramToCFG :: [Bind CoreBndr] -> IO [Function]
translateCoreProgramToCFG r =
    pure $ concat $ map countBindings r

getPivotName :: Var -> String
getPivotName = nameStableString . idName

countFlows :: CoreExpr -> [Function]
countFlows (Var x) = []
countFlows (Lit x) = []
countFlows (Type _) = []
countFlows (Coercion _) = []
countFlows (App f a) = countFlows f ++ countFlows a
countFlows (Lam x e) = countFlows e
countFlows (Let b e) = countFlows e ++ countBindingsInternal b
countFlows (Case e x t alts) = (concat (map countAlt alts))
countFlows (Cast e _) = countFlows e
countFlows (Tick _ e) = countFlows e

countBindings :: CoreBind -> [Function]
countBindings (NonRec binds expr) = [Function (getPivotName binds) $ countFlows expr]
countBindings (Rec bs) =
    map (\(binds,expr) -> Function (getPivotName binds) $ countFlows expr) bs

countBindingsInternal :: CoreBind -> [Function]
countBindingsInternal (NonRec binds expr) = countFlows expr
countBindingsInternal (Rec bs) = concat (map (countFlows . snd) bs)

countAlt :: (AltCon, [Var], CoreExpr) -> [Function]
countAlt (p, [], e) = [Function ((showSDocUnsafe $ ppr p) Prelude.<> " <<->> " Prelude.<> (showSDocUnsafe $ ppr e) ) $ countFlows e]
countAlt (p, val, e) = [Function ((showSDocUnsafe $ ppr p) Prelude.<> " <<->> " Prelude.<> (showSDocUnsafe $ ppr val) ) $ countFlows e]