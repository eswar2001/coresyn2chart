module Main where

import System.Directory ( doesDirectoryExist, listDirectory, createDirectoryIfMissing , removeFile)
import System.FilePath ( (</>) )
import Control.Monad (forM, unless, when)
import Data.List (isSuffixOf ,isPrefixOf )
import Syn2Chart.Types ( LBind, Function(Function) )
import Data.Aeson ( encode, eitherDecodeStrict )
import Data.ByteString.Lazy (toStrict)
import Syn2Chart.Traversal ( translateCoreProgramToCFG )
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString as DBS
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack,Text,pack,unpack)
import Control.Exception ( catch, throwIO )
import System.Directory.Internal.Prelude (isDoesNotExistError)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Concurrent.MVar
    ( putMVar, readMVar, takeMVar, newMVar, MVar )
import Options.Applicative
    ( (<**>),
      auto,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      strOption,
      value,
      execParser,
      helper,
      Parser )

getBase64FunctionName :: String -> String
getBase64FunctionName = unpack . decodeUtf8 . BS.encode . toStrict . encode

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

data CoreSyn2Chart = CoreSyn2Chart
  { path      :: String
  , function      :: String
  , pattern :: String
  , clean :: Bool
  }
  deriving (Show)

cliOptions :: Parser CoreSyn2Chart
cliOptions = CoreSyn2Chart
      <$> strOption
            ( long "path"
              <> short 'p'
              <> metavar "STRING"
              <> value "/tmp/coresyn2chart/"
              <> help "path of plugin dump"
            )
      <*> option auto
          ( long "function"
              <> short 'f'
              <> metavar "STRING"
              <> value "all"
              <> help "which function to process"
          )
      <*> option auto
          ( long "pattern"
              <> short 'i'
              <> metavar "STRING"
              <> value ""
              <> help "filter functions based on infix pattern"
          )
      <*> option auto
          ( long "clean"
              <> short 'd'
              <> metavar "Bool"
              <> value True
              <> help "delete the existing dump"
          )

main :: IO ()
main = do
  opts <- execParser (info (cliOptions <**> helper) (fullDesc <> progDesc " Tool to parse coresyn2chart dumps " <> header "coresyn2chart - an exe to generate paths for a flow from the dump generated by the plugin" ))
  let prefixPath = path opts
      functionName = function opts
      shouldDeleteDump = clean opts
      functionFilterInfix = pattern opts

  files <- getDirectoryContentsRecursive prefixPath

  let jsonFiles = filter (".lbind.ast.show.json" `isSuffixOf`) files
  print ("found " <> show (Prelude.length jsonFiles) <> " files")

  createDirectoryIfMissing True prefixPath

  print ("loading the files from dump" :: String)
  binds <- forM jsonFiles (`processDumpFiles` prefixPath)

  let hmBinds = HM.fromList $ filter (not . shouldFilter) $ concat binds

  case functionName of
    "all" -> do
      when shouldDeleteDump $ do
        removeIfExists (prefixPath <> "data.jsonL")
        removeIfExists (prefixPath <> "data-lbind.jsonL")
        removeIfExists (prefixPath <> "top-lvl-binds.json")
        removeIfExists (prefixPath <> "function-flows-cnt.txt")

      print ("created the top-lvl-binds for reference at: " <> prefixPath)
      DBS.writeFile (prefixPath <> "top-lvl-binds.json") (toStrict $ encode $ HM.keys hmBinds)

      print ("No specific function is passed so , generating for all " <> show (Prelude.length (HM.keys hmBinds)) <> "top-level-binds")
      mapM_ (\(name,functionData) -> do
        print ("processing function: " <> (unpack name))
        DBS.appendFile (prefixPath <> "data-lbind.jsonL") (toStrict (encode functionData) Prelude.<> "\n")
        pathsMvar <- newMVar 0
        convertBindToEdgesList prefixPath functionData hmBinds pathsMvar
        paths <- readMVar pathsMvar
        DBS.appendFile (prefixPath <> "function-flows-cnt.txt") (toStrict $ encode $ "got " <> show paths <> " paths for the function: " <> (unpack name) <> "\n")
        ) (filter (\(k,_) -> (pack functionFilterInfix) `T.isInfixOf` k) $ HM.toList hmBinds)
    x ->
      case HM.lookup (pack x) hmBinds of
        Just bind -> do
          print ("processing function: " <> x)
          DBS.appendFile (prefixPath <> "data-lbind.jsonL") (toStrict (encode bind) Prelude.<> "\n")
          pathsMvar <- newMVar 0
          convertBindToEdgesList prefixPath bind hmBinds pathsMvar
          paths <- readMVar pathsMvar
          DBS.appendFile (prefixPath <> "function-flows-cnt.txt") ((toStrict $ encode $ "got " <> show paths <>  " paths for the function: " <> x) <> "\n")
        Nothing -> print ("function not found , can you pick from this file: " <> prefixPath <> "top-lvl-binds.json")
  where
    shouldFilter x =
      let n =  fst x
      in ("$_in$$" `T.isInfixOf` n || "$_sys$" `T.isInfixOf` n || "$$" `T.isInfixOf` n)

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

processDumpFiles :: String -> String -> IO [(Text,Function)]
processDumpFiles file _ = do
  content <- DBS.readFile file
  case eitherDecodeStrict content of
    Right (binds :: [LBind]) ->
      mapM (\functionData@(Function _name _type _ _ _) -> pure (_name,functionData)) (translateCoreProgramToCFG binds)
    Left err -> do
      print err
      print file
      pure []

type Edge = (Text,Bool)

checkForErrorFlows :: [Edge] -> Bool
checkForErrorFlows = any (\edge -> any (\x -> x `T.isInfixOf` fst edge) errorFlow)
  where
    errorFlow :: [Text]
    errorFlow = ["throwException"]

shouldExpandThese :: Edge -> Bool
shouldExpandThese (name,_) = not $ any (\(_,func) -> func `T.isInfixOf` name) [("euler-hs" :: [Char],"forkFlow")]

functionsToFilterFromPath :: Edge -> Bool
functionsToFilterFromPath (_,True) = True
functionsToFilterFromPath (name,False) = any (`T.isInfixOf` name) ["$_sys$"]

convertBindToEdgesList :: String -> Function -> HM.HashMap Text Function -> MVar Int -> IO ()
convertBindToEdgesList prefixPath (Function pName _ pIsCase pChildren _) hmBinds mvar =
  go [(pName,pIsCase)] pChildren []
  where
    go :: [Edge] -> [Function] -> [Function] -> IO ()
    go prev [] [] =
      unless (checkForErrorFlows prev) $ do
        l <- takeMVar mvar
        print (pName,l)
        DBS.appendFile (prefixPath <> "data.jsonL") (toStrict $ encode prev Prelude.<> "\n")
        putMVar mvar (l + 1)
    go prev [] futureFunctions = go prev futureFunctions []
    go prev currentNodes futureFunctions =
      mapM_ (\(Function _name _type isCase childChildren _) ->
        unless (checkForErrorFlows [(_name,isCase)]) $ do
          if shouldExpandThese (_name,isCase)
            then
              case HM.lookup _name hmBinds of
                Just (Function __name __type _isCase _childChildren _) -> do
                  if _name /= pName
                    then go (prev <> [(__name,_isCase)]) _childChildren (childChildren <> futureFunctions)
                    else go (prev <> [(_name,isCase)]) childChildren futureFunctions
                Nothing -> go (prev <> [(_name,isCase)]) childChildren futureFunctions
            else go (prev <> [(_name,isCase)]) childChildren futureFunctions
      ) currentNodes