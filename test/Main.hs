{-# LANGUAGE ConstraintKinds #-}
module Main (main,demo) where

import System.Environment

main :: IO ()
main = do
    x <- getArgs
    if (length x == 0) && (length x == 1 || length x == 2) && (length x == 3)
      then
        case (length x) of
            0 -> print "$ headMaybe x"
            2 -> putStrLn $ "here_"
            3 -> putStrLn $ "here_"
            _ -> case headMaybe x of
                  Just x -> case (length x) + 100 of
                              100 -> print "100 + $ headMaybe x"
                  _ -> print "HI"
      else print "HI"
    --     4 -> putStrLn $ test1 $ test2 (head x)
    --     _ -> putStrLn $ test2 (head $ tail x)
    -- x <- maybe (pure "es") pure ()
    -- print x

headMaybe :: [String] -> Maybe String
headMaybe [] = Nothing
headMaybe (x: _) = Just x
-- [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "True" "IO ()" True [Function "ds_d1G7" "IO ()" True [Function "DEFAULT" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "[]" "IO ()" True [Function "$base$System.IO$putStrLn" "String -> IO ()" False [Function "$ghc-prim$GHC.CString$unpackCString#" "" False [Function "\"here\"#" "" False []]]]]]]]]],Function "True" "IO ()" True [Function "ds_d1G7" "IO ()" True [Function "DEFAULT" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False [Function "eqString ds_d1G6 (unpackCString# \"ag\"#)" "IO ()" True [Function "False" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "True" "IO ()" True [Function "ds_d1G7" "IO ()" True [Function "DEFAULT" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "[]" "IO ()" True [Function "$base$System.IO$putStrLn" "String -> IO ()" False [Function "$ghc-prim$GHC.CString$unpackCString#" "" False [Function "\"here\"#" "" False []]]]]]]]]],Function "[]" "IO ()" True [Function "$base$System.IO$putStrLn" "String -> IO ()" False [Function "$ghc-prim$GHC.CString$unpackCString#" "" False [Function "\"here\"#" "" False [Function "eqString ds_d1G6 (unpackCString# \"ag\"#)" "IO ()" True [Function "False" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "True" "IO ()" True [Function "ds_d1G7" "IO ()" True [Function "DEFAULT" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "[]" "IO ()" True [Function "$base$System.IO$putStrLn" "String -> IO ()" False [Function "$ghc-prim$GHC.CString$unpackCString#" "" False [Function "\"here\"#" "" False []]]]]]]]]]]]]]]]]

-- test :: Int -> String
-- test 10 = (<> "HI") "HI"
-- test 0 = (<> "HIHIIIII") "HI"
-- test _ = (<> "HIHIHI") "HI"

-- data X = Y | Z
-- 	deriving Show


test2 :: String -> String
test2 x = do
  case x of
    "Y" -> "Z"
    "Z" -> "Y"
    _ -> "A"
  "HI"

test1 :: String -> String
test1 x = case x of
  "Z" -> "X"
  "Y" -> "1"
  _ -> "2"

demo :: Maybe String -> IO ()
demo (Just val) = 
  case length val of
    0 -> print $ "zero"
    1 -> print $ try 1  <> "one"
    2 -> print $ try 10 <> "two"
demo Nothing = print "Nothing"


try :: Int -> String
try 100 = "100"
try 10 = "00"
try 1 = "0"

-- -- p :: a -> a -> a -> a -> [a]
-- p a b c d = [a ,b ,c, d]

-- p' :: (Monoid a) => a -> a -> a -> a -> a
-- p' a b c d = mempty