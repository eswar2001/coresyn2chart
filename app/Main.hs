module Main where

import System.Directory ( doesDirectoryExist, listDirectory, createDirectoryIfMissing , removeFile)
import System.FilePath ( (</>) )
import System.Environment ( getArgs )
import Control.Monad ( forM_ , forM,void,foldM,replicateM,when)
import Data.List (isInfixOf, isSuffixOf ,isPrefixOf ,foldl', null, intercalate,nub,any)
import Syn2Chart.Types ( LBind, Function(Function) )
import Data.Aeson
import Data.ByteString.Lazy (readFile, toStrict, fromStrict)
import Syn2Chart.Traversal
import qualified Data.Functor
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString as DBS
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack,Text,pack)
import System.IO (IOMode(WriteMode))
import System.IO.Extra (withBinaryFile)
import Data.ByteString.Builder
import System.Directory.Extra (removeFile)
import Control.Exception ( catch, throwIO )
import System.Directory.Internal.Prelude (isDoesNotExistError)
import qualified Data.HashMap.Strict as HM
import Debug.Trace
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Control.Concurrent.MVar
import Control.Concurrent
-- import Options.Applicative

getBase64FunctionName :: String -> String
getBase64FunctionName = unpack . decodeUtf8 . BS.encode . toStrict . encode

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- data CoreSyn2Chart = CoreSyn2Chart
--   { path      :: String
--   , function      :: String
--   }

-- sample :: Parser CoreSyn2Chart
-- sample = CoreSyn2Chart
--       <$> strOption
--             ( long "path"
--               <> short 'p'
--               <> metavar "FILE"
--               <> value "/tmp/coresyn2chart/"
--               <> help "path of plugin dump" )
--       <*> option auto
--           ( long "function"
--               <> short 'p'
--               <> metavar "FILE"
--              <> help "How enthusiastically to greet"
--              <> metavar "INT" )

main :: IO ()
main = do
  args <- getArgs
  let prefixPath =
              case args of
                []  -> "/tmp/coresyn2chart/"
                [x] -> x
                _ -> error "unexpected no of arguments"
  files <- getDirectoryContentsRecursive prefixPath
  let jsonFiles = filter (".lbind.ast.show.json" `isSuffixOf`) files
  print $ Prelude.length jsonFiles
  createDirectoryIfMissing True prefixPath
  removeIfExists (prefixPath <> "data.jsonL")
  removeIfExists (prefixPath <> "data-lbind.jsonL")
  removeIfExists (prefixPath <> "data-error.jsonL")
  binds <- forM (jsonFiles) (\x -> processDumpFiles x prefixPath)
  let hmBinds = HM.fromList $ filter (\x -> not $ shouldFilter x) $ concat binds
  DBS.writeFile (prefixPath <> "top-lvl-binds.json") (toStrict $ encode $ HM.keys hmBinds)
  case HM.lookup "$main$Main$main" hmBinds of
    Just bind -> do
      print ("processing function: " <> "$main$Main$main")
      DBS.appendFile (prefixPath <> "data-lbind.jsonL") (toStrict (encode $ bind) Prelude.<> "\n")
      pathsMvar <- newMVar (0)
      visitedMVar <- newMVar ([] :: [String])
      convertBindToEdgesList prefixPath bind hmBinds pathsMvar visitedMVar
      paths <- readMVar pathsMvar
      print ("got " <> show paths <>  " paths for the function: " <> "$main$Main$main")
      -- mapM_ (\x -> do
      --     DBS.appendFile (prefixPath <> "data.jsonL") (toStrict $ (encode x) Prelude.<> "\n" )
      --   ) $ paths
    Nothing -> pure ()
  -- mapM_ (\(name,functionData) -> do
  --     print ("processing function: " <> name)
  --     DBS.appendFile (prefixPath <> "data-lbind.jsonL") (toStrict (encode $ functionData) Prelude.<> "\n")
  --     pathsMvar <- newMVar (0)
  --     paths <- convertBindToEdgesList prefixPath functionData hmBinds pathsMvar
  --     -- paths <- readMVar pathsMvar
  --     print ("got " <> (show $ (length paths)) <> " paths for the function: " <> name)
  --     mapM_ (\x -> do
  --         DBS.appendFile (prefixPath <> "data.jsonL") (toStrict $ (encode x) Prelude.<> "\n" )
  --       ) $ paths
  --   ) (HM.toList $ hmBinds)
  pure ()
  where
    shouldFilter x =
      let n =  fst x
      in if ("$_in$$" `isInfixOf` (n) || "$_sys$" `isInfixOf` (n) || "$$" `isInfixOf` (n)) then True else False

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    names <- listDirectory dir
    paths <- forM names $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getDirectoryContentsRecursive path
            else return [path]
    return (concat paths)

processDumpFiles :: String -> String -> IO [(String,Function)]
processDumpFiles file prefixPath = do
  content <- DBS.readFile file
  case eitherDecodeStrict $ content of
    Right (binds :: [LBind]) -> do
      mapM_ print binds
      mapM (\functionData@(Function _name _type _ _) -> pure $ (_name,functionData)) (translateCoreProgramToCFG binds)
    Left err -> do
      print err
      print file
      pure []

type Edge = (Text,Bool)

checkForErrorFlows :: [Edge] -> Bool
checkForErrorFlows edges =
  any (\edge -> any (\x -> x `T.isInfixOf` (fst edge)) errorFlow) $ edges
  where
    errorFlow :: [Text]
    errorFlow = ["throwException","throwExceptionV2"]

shouldExpandThese :: Edge -> Bool
shouldExpandThese (name,_) = not $ any (\(pkg,func) -> func `T.isInfixOf` name) [("euler-hs","forkFlow")]

functionsToFilterFromPath :: Edge -> Bool
functionsToFilterFromPath (name,True) = True
functionsToFilterFromPath (name,False) = any (\x -> x `T.isInfixOf` name) ["$_sys$"]

convertBindToEdgesList :: String -> Function -> HM.HashMap String Function -> MVar (Int) -> MVar [String] -> IO ()
convertBindToEdgesList prefixPath root@(Function pName pType pIsCase pChildren) hmBinds mvar visitedMVar =
  go [(pack pName,pIsCase)] pChildren []
  where
    go :: [Edge] -> [Function] -> [Function] -> IO ()
    go prev [] [] =
      when (not $ checkForErrorFlows prev) $ do
        l <- takeMVar mvar
        print (pName,l)
        putMVar mvar (l + 1)
        DBS.appendFile (prefixPath <> "data.jsonL") (toStrict $ encode (prev) Prelude.<> "\n")
    go prev [] futureFunctions = go prev futureFunctions []
    go prev currentNodes futureFunctions = do
      mapM_ (\(Function _name _type isCase childChildren) -> do
        -- print(_name,checkForErrorFlows [(pack _name,isCase)])
        when (checkForErrorFlows [(pack _name,isCase)]) $ do
            -- visitedList <- takeMVar visitedMVar
            -- when (elem _name visitedList) $ print ("visited",_name)
            -- putMVar visitedMVar (visitedList <>[_name])
            if shouldExpandThese (pack _name,isCase)
              then
                case HM.lookup _name hmBinds of
                  Just bind@(Function __name __type _isCase _childChildren) -> do
                    if (_name /= pName)
                      then do
                        DBS.appendFile (prefixPath <> "data-lbind.jsonL") (toStrict (encode $ bind) Prelude.<> "\n")
                        go (prev <> [(pack __name,_isCase)]) _childChildren (childChildren <> futureFunctions)
                      else go (prev <> [(pack _name,isCase)]) childChildren futureFunctions
                  Nothing -> go (prev <> [(pack _name,isCase)]) childChildren futureFunctions
              else go (prev <> [(pack _name,isCase)]) childChildren futureFunctions
          
        ) (currentNodes)

    -- go' :: [Edge] -> [Function] -> [Function] -> IO [[Edge]]
    -- go' prev [] [] = [prev]
    -- go' prev [] futureFunctions = go' prev futureFunctions []
    -- go' prev currentNodes [] =
    --   mapM (\(Function _name _ isCase childChildren) -> do
    --     case HM.lookup _name hmBinds of
    --       Just (Function __name __type _isCase _childChildren) -> go' (prev <> [(pack __name,_isCase)]) _childChildren (childChildren)
    --       Nothing -> go' (prev <> [(pack _name,isCase)]) childChildren []
    --     ) currentNodes
    -- go' prev currentNodes futureFunctions =
    --   mapM (\(Function _name _ isCase childChildren) -> do
    --     case HM.lookup _name hmBinds of
    --       Just (Function __name __type _isCase _childChildren) -> do
    --         res <- go' (prev <> [(pack __name,_isCase)]) _childChildren (childChildren)
    --         mapM () go futureFunctions
    --       Nothing -> go' (prev <> [(pack _name,isCase)]) childChildren []
    --     ) currentNodes

      -- void $ mapM (\(Function _name _ isCase childChildren) -> do
      --   case HM.lookup _name hmBinds of
      --     Just (Function __name __type _isCase _childChildren) -> go (prev <> [(pack __name,_isCase)]) _childChildren (childChildren <> futureFunctions)
      --     Nothing -> go (prev <> [(pack _name,isCase)]) childChildren futureFunctions
      --   ) currentNodes

-- convertBindToEdgesList :: Function -> HM.HashMap String Function -> IO [String]
-- convertBindToEdgesList root@(Function pName pType pIsCase pChildren) hmBinds = go root []
--   where
--     go :: Function -> [String] -> IO [String]
--     go (Function _name _ isCase []) [] = do
--       pure $ [show (_name,isCase)]
--     go (Function _name _ isCase []) accg = do
--       pure $ map (\x -> (show (_name,isCase)) <> x) accg
--     -- go (Function _name _ isCase childChildren) [] = do
--     --   case HM.lookup _name hmBinds of
--     --     Just val -> do
--     --       accgb <- go val []
--     --       r <- mapM (\x -> do
--     --               path <- go x accgb
--     --               pure $ map (\x -> (show (_name,isCase)) <> "," <> x) path
--     --             ) childChildren
--     --       pure $ concat r
--     --     Nothing -> do
--     --       rr <- mapM (\x -> do
--     --                 r <- go x []
--     --                 pure $ map (\x -> show (_name,isCase) <> "," <> x)  r
--     --             ) childChildren
--     --       pure $ concat rr
--     go (Function _name _ isCase childChildren) accg = do
--       case HM.lookup _name hmBinds of
--         Just val -> do
--           accgb <- go val accg
--           r <- mapM (\x -> do
--                   path <- go x accgb
--                   pure $ map (\x -> (show (_name,isCase)) <> "," <> x) path
--                 ) childChildren
--           pure $ concat r
--         Nothing -> do
--           r <- mapM (\x -> do
--                   path <- go x accg
--                   pure $ map (\x -> (show (_name,isCase)) <> "," <> x) path
--                 ) childChildren
--           pure $ concat r
--     go (Function _name _ isCase [childChildren]) accg = do
--       case HM.lookup _name hmBinds of
--         Just val -> do
--           accgb <- go val accg
--           r <- go childChildren accgb
--           pure $ map (\x -> (show (_name,isCase)) <> "," <> x) r
--         Nothing -> do
--           r <- go childChildren accg
--           pure $ map (\x -> (show (_name,isCase)) <> "," <> x) r

    -- go (Function _name _ False childChildren) accg =
    --   r <- foldM (\acc -> go childChildren acc) accg
    --   pure $ [(_name,False)] <> r


-- go (Function _name _ isCase childChildren) acc =
--   if isCase
--     then do
--       -- acc1 <- go f1 acc0
--       -- acc2 <- go f2 acc0
--       -- acc1 ++ acc2
--     else foldl (\acc c -> go c acc) acc  childChildren
--       -- acc1 <- go f1 acc0
--       -- acc2 <- go f2 acc1

{-
main = do
  f1
  f2
  f3
  case x of
    Just 
    Nothing 

f1 - case 
-}

    -- go :: [Edge] -> [Function] -> [Function] -> IO ()
    -- go prev [] [] = do
    --   l <- takeMVar mvar
    --   print (pName,l)
    --   DBS.appendFile (prefixPath <> "data.jsonL") (toStrict $ encode ([prev]) Prelude.<> "\n" )
    --   putMVar mvar (l + 1)
    -- go prev [] futureFunctions = go prev futureFunctions []
    -- go prev currentNodes futureFunctions = do
    --   mapM (\(Function _name _ isCase childChildren,doneVar) -> do
    --     case HM.lookup _name hmBinds of
    --       Just (Function __name __type _isCase _childChildren) -> go (prev <> [(pack __name,_isCase)]) _childChildren (childChildren <> futureFunctions)
    --       Nothing -> go (prev <> [(pack _name,isCase)]) childChildren futureFunctions
    --     ) (currentNodes)

    -- go :: [Edge] -> [Function] -> [Function] -> IO [String]
    -- go prev [] [] = pure $ [show $ prev]
    -- go prev [] futureFunctions = go prev futureFunctions []
    -- go prev currentNodes futureFunctions = do
    --   res <- mapM (\(Function _name _ isCase childChildren) -> do
    --     resp <- case HM.lookup _name hmBinds of
    --       Just (Function __name __type _isCase _childChildren) -> go (prev <> [(pack __name,_isCase)]) _childChildren (childChildren <> futureFunctions)
    --       Nothing -> go (prev <> [(pack _name,isCase)]) childChildren futureFunctions
    --     pure $ resp
    --     ) (currentNodes)
    --   pure $ concat res