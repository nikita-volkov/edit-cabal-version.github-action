import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.HappyPathIO
import Coalmine.Prelude
import qualified Data.Text.IO as TextIO
import qualified EditCabalVersion as Editor

main :: IO ()
main = do
  Config workDir action <- loadConfig
  Path.setCurrentDirectory workDir
  path <- findCabalFile
  let pathString = Path.toString path
  contents <- TextIO.readFile pathString
  (before, after) <- case action of
    GetConfigAction -> do
      version <- case Editor.getVersion contents of
        Left err -> die [i|Failed to read file $path: $err|]
        Right res -> return res
      return (version, version)
    SetConfigAction version -> do
      Editor.ModifiedResult {..} <- case Editor.setVersion version contents of
        Left err -> die [i|Failed to read file $path: $err|]
        Right res -> return res
      TextIO.writeFile pathString modifiedResultText
      return (modifiedResultOldVersion, modifiedResultNewVersion)
    BumpConfigAction position -> do
      Editor.ModifiedResult {..} <- case Editor.bumpVersion position contents of
        Left err -> die [i|Failed to read file $path: $err|]
        Right res -> return res
      TextIO.writeFile pathString modifiedResultText
      return (modifiedResultOldVersion, modifiedResultNewVersion)
  putStrLn [i|::set-output name=before::$before|]
  putStrLn [i|::set-output name=after::$after|]

findCabalFile :: IO Path
findCabalFile = do
  files <- filter ((==) ["cabal"] . Path.extensions) <$> Path.listDirectory "."
  case files of
    [file] -> return file
    [] -> die "No Cabal-file found"
    _ -> die "More than one Cabal-file found"

loadConfig :: IO Config
loadConfig = do
  workDir <- fromMaybe "." <$> loadNonRequiredEnv "work-dir"
  mode <- loadRequiredEnv @Text "mode"
  action <- case mode of
    "get" -> getAction
    "set" -> setAction
    "bump" -> bumpAction
    "read" -> getAction
    _ -> die "Unexpected mode"
  return $ Config workDir action
  where
    getAction = return $ GetConfigAction
    bumpAction = do
      place <- loadRequiredEnv "bump-place"
      if place < 0
        then die "bump-place is smaller than 0"
        else
          if place > 7
            then die "bump-place is larger than 7"
            else return ()
      return $ BumpConfigAction place
    setAction = do
      value <- loadRequiredEnv "set-value"
      return $ SetConfigAction value

-- * Config

data Config
  = Config !Path !ConfigAction

data ConfigAction
  = GetConfigAction
  | BumpConfigAction !Int
  | SetConfigAction !NumericVersion
