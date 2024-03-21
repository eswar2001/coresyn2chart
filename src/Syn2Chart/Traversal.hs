module Syn2Chart.Traversal where
import Syn2Chart.Types
    ( Function(..),
      LBind(..),
      LAltCon,
      LExpr(..),
      extractNameFromLAltCon )
import Data.Text (Text)

translateCoreProgramToCFG :: [LBind] -> [Function]
translateCoreProgramToCFG = concatMap traverseBindings

traverseForFlowsLExpr :: LExpr -> [Function] -> [Function]
traverseForFlowsLExpr (LVar name tykind srcSpan isLocal isExported) argument = [Function name tykind False argument (Just srcSpan)]
traverseForFlowsLExpr x@LLit {} argument = argument
traverseForFlowsLExpr (LType _) argument = argument
traverseForFlowsLExpr (LApp (LVar name tykind srcSpan isLocal isExported) (LLit typeOfLit val isFunc)) argument = [Function name "" False [Function val typeOfLit False argument Nothing] Nothing]
traverseForFlowsLExpr (LApp (LVar name tykind srcSpan isLocal isExported) (LVar name1 tykind1 srcSpan1 isLocal1 isExported1)) argument = [Function name tykind False [Function name1 tykind1 False argument (Just srcSpan1)] (Just srcSpan)]
traverseForFlowsLExpr (LApp (LVar name tykind srcSpan isLocal isExported) (LType _)) argument = [Function name tykind False argument (Just srcSpan)]
traverseForFlowsLExpr (LApp f a) argument =
    let arg = traverseForFlowsLExpr a argument
        fun = traverseForFlowsLExpr f arg
    in fun
traverseForFlowsLExpr (LLam e a) argument = [Function e "" False (traverseForFlowsLExpr a argument) Nothing]
traverseForFlowsLExpr (LLet b e) argument =
  let arg = traverseForFlowsLExpr e argument
  in traverseBindingsInternal b arg
traverseForFlowsLExpr (LCase _ pprE _ t alts) argument = [CaseFunction pprE t True (concatMap (\x -> countAlt t x argument) alts) Nothing]
traverseForFlowsLExpr (LUnhandled _ _) argument = argument

traverseBindings :: LBind -> [Function]
traverseBindings (LNonRec name tyVarK expr) =
  [Function name tyVarK False (traverseForFlowsLExpr expr []) Nothing]
traverseBindings (LRec bs) =
  map (\(name,tyVarK,expr) -> Function name tyVarK False (traverseForFlowsLExpr expr []) Nothing) bs
traverseBindings _ = []

traverseBindingsInternal :: LBind -> [Function] -> [Function]
traverseBindingsInternal (LNonRec _ _ expr) argument = traverseForFlowsLExpr expr argument
traverseBindingsInternal (LRec bs) argument = concatMap (\(_,_,expr) -> traverseForFlowsLExpr expr argument) bs
traverseBindingsInternal LNull argument = argument

countAlt :: Text -> (LAltCon, [LExpr], LExpr) -> [Function] -> [Function]
countAlt t (p, [], e) args = [CaseRelation (extractNameFromLAltCon p) t True (traverseForFlowsLExpr e args) Nothing]
countAlt t (p, [LVar name tykind srcSpan isLocal isExported], e) args = [CaseRelation (extractNameFromLAltCon p) t True (traverseForFlowsLExpr e args) (Just srcSpan)]
countAlt t (p, (LVar name tykind srcSpan isLocal isExported):xs, e) args = [CaseRelation (extractNameFromLAltCon p) t True (traverseForFlowsLExpr e args) (Just srcSpan) ]
countAlt t (p, _, e) args = [CaseRelation (extractNameFromLAltCon p) t True (traverseForFlowsLExpr e args) Nothing]