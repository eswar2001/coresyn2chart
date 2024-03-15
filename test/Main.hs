{-# LANGUAGE ConstraintKinds #-}
module Main (main) where

import Control.Monad
import Data.Functor
import System.Environment

main :: IO ()
main = do
    x <- getArgs
    case length x of
        0 -> putStrLn $ "here__"
        2 -> putStrLn $ "here_"
        4 -> putStrLn $ test1 $ test2 (head x)
        _ -> putStrLn $ test2 (head $ tail x)
    -- case (headMaybe x) of
    --   Just y -> print y
    --   Nothing -> putStrLn "here" *> putStrLn "here2"
    maybe (putStrLn "here" *> putStrLn "here2") (print) (headMaybe x)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x: xs) = Just x
-- [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "True" "IO ()" True [Function "ds_d1G7" "IO ()" True [Function "DEFAULT" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "[]" "IO ()" True [Function "$base$System.IO$putStrLn" "String -> IO ()" False [Function "$ghc-prim$GHC.CString$unpackCString#" "" False [Function "\"here\"#" "" False []]]]]]]]]],Function "True" "IO ()" True [Function "ds_d1G7" "IO ()" True [Function "DEFAULT" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False [Function "eqString ds_d1G6 (unpackCString# \"ag\"#)" "IO ()" True [Function "False" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "True" "IO ()" True [Function "ds_d1G7" "IO ()" True [Function "DEFAULT" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "[]" "IO ()" True [Function "$base$System.IO$putStrLn" "String -> IO ()" False [Function "$ghc-prim$GHC.CString$unpackCString#" "" False [Function "\"here\"#" "" False []]]]]]]]]],Function "[]" "IO ()" True [Function "$base$System.IO$putStrLn" "String -> IO ()" False [Function "$ghc-prim$GHC.CString$unpackCString#" "" False [Function "\"here\"#" "" False [Function "eqString ds_d1G6 (unpackCString# \"ag\"#)" "IO ()" True [Function "False" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "True" "IO ()" True [Function "ds_d1G7" "IO ()" True [Function "DEFAULT" "IO ()" True [Function "$_sys$fail" "" False [Function "$ghc-prim$GHC.Prim$void#" "" False []]],Function "[]" "IO ()" True [Function "$base$System.IO$putStrLn" "String -> IO ()" False [Function "$ghc-prim$GHC.CString$unpackCString#" "" False [Function "\"here\"#" "" False []]]]]]]]]]]]]]]]]

-- test :: Int -> String
-- test 10 = (<> "HI") "HI"
-- test 0 = (<> "HIHIIIII") "HI"
-- test _ = (<> "HIHIHI") "HI"

-- data X = Y | Z
-- 	deriving Show


test2 :: String -> String
test2 x = case x of
  "Y" -> "Z"
  "Z" -> "Y"
  _ -> "A"

test1 :: String -> String
test1 x = case x of
  "Z" -> "X"
  "Y" -> "1"
  _ -> "2"

-- -- p :: a -> a -> a -> a -> [a]
-- p a b c d = [a ,b ,c, d]

-- p' :: (Monoid a) => a -> a -> a -> a -> a
-- p' a b c d = mempty
