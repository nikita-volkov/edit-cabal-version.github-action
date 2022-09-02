import qualified Coalmine.ArgsParser as ArgsParser
import qualified Coalmine.EvenSimplerPaths as Path
import qualified Coalmine.HappyPathIO as HappyPathIO
import Coalmine.Prelude
import qualified Data.Text.IO as TextIO
import EditCabalVersion

main :: IO ()
main = do
  Config workDir action <- readArgs
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

readArgs :: IO Config
readArgs =
  ArgsParser.getAndConsumeArgsHappily mode
  where
    mode = do
      workDir <- ArgsParser.parsed "Path" lenientParser
      action <-
        join . ArgsParser.enum $
          [ ("get", get),
            ("bump", bump)
          ]
      return $ Config workDir action
      where
        get = return GetConfigAction
        bump = do
          position <- ArgsParser.int 0 7
          return $ BumpConfigAction position

-- * Config

data Config
  = Config !Path !ConfigAction

data ConfigAction
  = GetConfigAction
  | BumpConfigAction !Int
