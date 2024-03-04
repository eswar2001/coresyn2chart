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
import Data.Bool


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
        let tree = foldl (\acc x@(Function name _ _ _) -> acc Prelude.<> "\n\n\n\n" Prelude.<> (drawTree $ toDataTree x)) "" res
        _ <- Prelude.writeFile (moduleLoc Prelude.<> ".syn.log") tree
        _ <- Data.ByteString.Lazy.writeFile (moduleLoc Prelude.<> ".syn.json") 
                (encodePretty $ Map.fromList $ map (\t@(Function name _type _ x) -> (name,t)) res)
        pure ()
    return guts

getFilePath :: SrcSpan -> String
getFilePath (RealSrcSpan rSSpan) = unpackFS $ srcSpanFile rSSpan
getFilePath (UnhelpfulSpan fs) =  unpackFS fs

data Function = Function String String Bool [Function]
    deriving (Show)

instance ToJSON Function where
    toJSON (Function name _type isCase f') =
        Object $ 
            HM.fromList [("name",toJSON name),("body",toJSON f'),("type",toJSON _type),("isCase",toJSON isCase)]

-- instance Semigroup Function where
--     (Function name1 type1 isCase1 children1) <> (Function name2 type2 isCase2 children2) =
--         if name1 == "" || name2 == "" then
--             Function (name1 ++ name2) (type1 ++ type2) (isCase1 && isCase2) (children1 Prelude.<> children2)
--         else
--             Function (name1 ++ "__" ++ name2) (type1 ++ "__" ++ type2) (isCase1 && isCase2) (children1 Prelude.<> children2)

-- instance Monoid Function where
--     mempty = Function "" "" False []

toDataTree :: Function -> Tree String
toDataTree (Function name _type isCase children) = Node (name ++ " :: @" ++ _type) (map toDataTree children)

treeToString :: Function -> String
treeToString (Function name _type isCase children) = name ++ " ::@" ++ _type ++ "\n" ++ concatMap (\child -> "  " ++ treeToString child) children

translateCoreProgramToCFG :: [Bind CoreBndr] -> IO [Function]
translateCoreProgramToCFG r =
    pure $ concat $ map countBindings r

getPivotName :: Var -> String
getPivotName var = nameStableString $ idName var

traverseForFlows :: CoreExpr -> [Function] -> [Function]
traverseForFlows xx@(Var x) _ =
    let name = (getPivotName x)
    in ([Function name (showSDocUnsafe $ ppr $ tyVarKind x) False []])
traverseForFlows (Lit x) _ = []
traverseForFlows t@(Type _) _ = []
traverseForFlows (Coercion _) _ = []
traverseForFlows (App (Var id) (Lit x)) argument = [Function (getPivotName id) "" False [Function (showSDocUnsafe $ ppr x) "" False argument]]
traverseForFlows (App (Var id) (Var x)) argument = [Function (getPivotName id) "" False [Function (traceShowId $ getPivotName x) "" False argument]]
-- traverseForFlows (App f (Type x)) = traverseForFlows f
traverseForFlows (App (Var id) (Type x)) argument = [Function (getPivotName id) "" False argument]
traverseForFlows (App f a) argument =
    (let arg = traverseForFlows (trace (show $ toConstr a) a) argument
         fun = traverseForFlows f arg
    in fun)
traverseForFlows x@(Lam e a) argument =
    let !_ = (getPivotName e)
    in [Function (getPivotName e) "" False (traverseForFlows a argument)]
traverseForFlows x@(Let b e) _ = ((traverseForFlows e []) ++ countBindingsInternal b)
traverseForFlows (Case e x t alts) _ = (concat (map (countAlt t) alts))
traverseForFlows x@(Cast e _) _ = ((traverseForFlows e []))
traverseForFlows x@(Tick _ e) _ = ((traverseForFlows e []))

countBindings :: CoreBind -> [Function]
countBindings (NonRec binds expr) =
  let maybeName = Just (nameStableString $ idName binds)
  in maybe [] (\name -> [Function name (showSDocUnsafe $ ppr $ tyVarKind binds) False $ traverseForFlows expr []]) maybeName
countBindings (Rec bs) =
  mapMaybe (\(binds,expr) ->
    let maybeName = Just (nameStableString $ idName binds)
    in maybe Nothing (\name -> Just $ Function name (showSDocUnsafe $ ppr $ tyVarKind binds) False $ traverseForFlows expr []) maybeName) bs

countBindingsInternal :: CoreBind -> [Function]
countBindingsInternal (NonRec binds expr) = traverseForFlows expr []
countBindingsInternal (Rec bs) = concat (map ((\x -> traverseForFlows x []) . snd) bs)

countAlt :: Type -> (AltCon, [Var], CoreExpr) -> [Function]
countAlt t (p, [], e) = [Function (showSDocUnsafe $ ppr p) (showSDocUnsafe $ ppr t) True $ traverseForFlows e []]
countAlt t (p, val, e) = [Function ((showSDocUnsafe $ ppr p) Prelude.<> " <<->> " Prelude.<> (showSDocUnsafe $ ppr val)) (showSDocUnsafe $ ppr t) True $ traverseForFlows e []]

