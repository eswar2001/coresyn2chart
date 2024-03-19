{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, DeriveDataTypeable, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Syn2Chart.Types where

import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import GhcPlugins
    ( AltCon(..),
      Expr(Lam, Lit, Var, Type, App, Let, Case),
      Var,
      CoreBind,
      Bind(Rec, NonRec),
      idName,
      nameStableString,
      showSDocUnsafe,
      Outputable(ppr) )
import qualified Data.Map as Map
import Data.Aeson
    ( FromJSON,
      ToJSON(toJSON),
      Value(String, Object),
      object,
      KeyValue((.=)) )
import Data.Data ( Data(toConstr) )
import Data.Text (pack, Text)
import Prelude hiding (id)

data Function = Function Text Text Bool [Function] (Maybe Text)
    deriving (Show)

instance ToJSON Function where
    toJSON (Function name _type isCase f' mSrcSpan) = Object $ HM.fromList [("name",toJSON name),("body",toJSON f'),("type",toJSON _type),("isCase",toJSON isCase),("srcSpan", toJSON mSrcSpan)]

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

data LBind = LNonRec Text Text LExpr
            | LRec [(Text, Text , LExpr)]
            | LNull
  deriving (Generic,Data,Show,ToJSON,FromJSON)

data LExpr
  = LVar   Text Text Text Bool Bool
  | LLit   Text Text Bool
  | LType  Text
  | LApp   LExpr LExpr
  | LLam   Text LExpr
  | LLet   LBind LExpr
  | LCase  LExpr Text Text Text [LAlt]
  | LUnhandled Text Text
  deriving (Generic,Data,Show,ToJSON,FromJSON)

type LAlt = (LAltCon, [LExpr], LExpr)

data LAltCon
  = LDataAlt Text
  | LLitAlt  Text
  | LDEFAULT
  deriving (Generic, Eq, Data,Show,ToJSON,FromJSON)

extractNameFromLAltCon :: LAltCon -> Text
extractNameFromLAltCon (LDataAlt name) = name
extractNameFromLAltCon (LLitAlt name) = name
extractNameFromLAltCon LDEFAULT = "DEFAULT"

bindToJSON :: CoreBind -> [(Text, Value)]
bindToJSON (NonRec binder ((Lam _ expr))) =
  [(pack $ nameStableString (idName binder), toJSON expr)]
bindToJSON (NonRec binder expr) =
  [(pack $ nameStableString (idName binder), toJSON expr)]
bindToJSON (Rec binds) =
  map (\(b, e) -> (pack $ nameStableString (idName b), toJSON e)) binds