import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.HappyPathIO
import Coalmine.Prelude
import qualified Data.Text.IO as TextIO
import EditCabalVersion

main :: IO ()
main = do
  Config workDir action <- loadConfig
  Path.setCurrentDirectory workDir
  path <- findCabalFile
  let pathString = Path.toString path
  contents <- TextIO.readFile pathString
  case action of
    BumpConfigAction position -> do
      VersionBumped {..} <- case bumpVersion position contents of
        Left err -> die [i|Failed to read file "$path": $err|]
        Right res -> return res
      TextIO.writeFile pathString versionBumpedText
      putStrLn [i|::set-output name=before::$versionBumpedOldVersion|]
      putStrLn [i|::set-output name=after::$versionBumpedNewVersion|]
    GetConfigAction -> do
      VersionBumped {..} <- case bumpVersion 0 contents of
        Left err -> die [i|Failed to read file "$path": $err|]
        Right res -> return res
      putStrLn [i|::set-output name=before::$versionBumpedOldVersion|]
      putStrLn [i|::set-output name=after::$versionBumpedOldVersion|]
    SetConfigAction version ->
      error "TODO"

editFile :: Path -> (Text -> IO Text) -> IO ()
editFile path onText =
  TextIO.readFile pathString >>= onText >>= TextIO.writeFile pathString
  where
    pathString = Path.toString path

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
    "read" -> getAction
    "bump" -> bumpAction
    "write" -> setAction
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
      value <- loadRequiredEnv "write-value"
      return $ SetConfigAction value

-- * Config

data Config
  = Config !Path !ConfigAction

data ConfigAction
  = GetConfigAction
  | BumpConfigAction !Int
  | SetConfigAction !NumericVersion
