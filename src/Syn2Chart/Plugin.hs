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
import Data.List.Extra (nub,isInfixOf)
import Data.Maybe
import Data.Tree
import Debug.Trace (traceShowId)
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
    _ <- liftIO $ do
        let binds = mg_binds guts
            moduleN = moduleNameString $ moduleName $ mg_module guts
            moduleLoc = getFilePath $ mg_loc guts
        res <- translateCoreProgramToCFG binds
        let tree = foldl (\acc x@(Function name _ _) -> acc Prelude.<> "\n\n\n\n" Prelude.<> (drawTree $ toDataTree x)) "" res
        _ <- Prelude.writeFile (moduleLoc Prelude.<> ".syn.log") tree
        _ <- Data.ByteString.Lazy.writeFile (moduleLoc Prelude.<> ".syn.json") 
                (encodePretty $ Map.fromList $ map (\t@(Function name _type x) -> (name,t)) res)
        pure ()
    return guts

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs

data Function = Function String String [Function]
    deriving (Show)

instance ToJSON Function where
    toJSON (Function name _type f') = 
        Object $ 
            HM.fromList [("name",toJSON name),("body",toJSON f'),("type",toJSON _type)]

instance Semigroup Function where
    (Function name1 type1 children1) <> (Function name2 type2 children2) =
        if name1 == "" || name2 == "" then
            Function (name1 ++ name2) (type1 ++ type2) (children1 Prelude.<> children2)
        else
            Function (name1 ++ "__" ++ name2) (type1 ++ "__" ++ type2) (children1 Prelude.<> children2)

instance Monoid Function where
    mempty = Function "" "" []

toDataTree :: Function -> Tree String
toDataTree (Function name _type children) = Node (name ++ " :: @" ++ _type) (map toDataTree children)

treeToString :: Function -> String
treeToString (Function name _type children) = name ++ " ::@" ++ _type ++ "\n" ++ concatMap (\child -> "  " ++ treeToString child) children

translateCoreProgramToCFG :: [Bind CoreBndr] -> IO [Function]
translateCoreProgramToCFG r =
    pure $ concat $ map countBindings r

getPivotName :: Var -> String
getPivotName = nameStableString . idName

traverseForFlows :: CoreExpr -> [Function]
traverseForFlows xx@(Var x) =
    let name = (getPivotName x)
    in [Function name (showSDocUnsafe $ ppr $ tyVarKind x) []]
traverseForFlows (Lit x) = []
traverseForFlows t@(Type _) = []
traverseForFlows (Coercion _) = []
traverseForFlows (App f a) =
    let arg = traverseForFlows a
        fun = case traverseForFlows f of
                [] -> []
                [(Function name _type [])] -> [Function name _type arg]
                [(Function name _type x)] -> [Function name _type (arg Prelude.<> x)]
                x -> x
    in fun
traverseForFlows (Lam e a) =
    let !_ = (getPivotName e)
    in [Function (getPivotName e) "" (traverseForFlows a)]
traverseForFlows (Let b e) = traverseForFlows e ++ countBindingsInternal b
traverseForFlows (Case e x t alts) = (concat (map (countAlt t) alts))
traverseForFlows (Cast e _) = traverseForFlows e
traverseForFlows (Tick _ e) = traverseForFlows e

countBindings :: CoreBind -> [Function]
countBindings (NonRec binds expr) = [Function (getPivotName binds) (showSDocUnsafe $ ppr $ tyVarKind binds) $ traverseForFlows expr]
countBindings (Rec bs) =
    map (\(binds,expr) -> Function (getPivotName binds) (showSDocUnsafe $ ppr $ tyVarKind binds) $ traverseForFlows expr) bs

countBindingsInternal :: CoreBind -> [Function]
countBindingsInternal (NonRec binds expr) = traverseForFlows expr
countBindingsInternal (Rec bs) = concat (map (traverseForFlows . snd) bs)

countAlt :: Type -> (AltCon, [Var], CoreExpr) -> [Function]
countAlt t (p, [], e) = [Function (showSDocUnsafe $ ppr p) (showSDocUnsafe $ ppr t) $ traverseForFlows e]
countAlt t (p, val, e) = [Function ((showSDocUnsafe $ ppr p) Prelude.<> " <<->> " Prelude.<> (showSDocUnsafe $ ppr val)) (showSDocUnsafe $ ppr t) $ traverseForFlows e]