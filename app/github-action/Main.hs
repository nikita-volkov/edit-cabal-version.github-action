import qualified Coalmine.ArgsParser as ArgsParser
import qualified Coalmine.EvenSimplerPaths as Path
import qualified Coalmine.HappyPathIO as HappyPathIO
import Coalmine.Prelude
import qualified Data.Text.IO as TextIO
import EditCabalVersion

main :: IO ()
main = do
  config <- readArgs
  path <- findCabalFile
  let pathString = Path.toString path
  contents <- TextIO.readFile pathString
  case config of
    BumpConfig position -> do
      VersionBumped {..} <- case bumpVersion position contents of
        Left err -> die [i|Failed to read file "$path": $err|]
        Right res -> return res
      TextIO.writeFile pathString versionBumpedText
      putStrLn [i|::set-output name=old-version::$versionBumpedOldVersion|]
      putStrLn [i|::set-output name=new-version::$versionBumpedNewVersion|]
    GetConfig -> do
      VersionBumped {..} <- case bumpVersion 0 contents of
        Left err -> die [i|Failed to read file "$path": $err|]
        Right res -> return res
      putStrLn [i|::set-output name=old-version::$versionBumpedOldVersion|]
      putStrLn [i|::set-output name=new-version::$versionBumpedOldVersion|]

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
  ArgsParser.getAndConsumeArgsHappily . join $ mode
  where
    mode =
      ArgsParser.enum
        [ ("get", get),
          ("bump", bump)
        ]
      where
        get = return GetConfig
        bump = do
          position <- ArgsParser.minMaxInt 0 7
          return $ BumpConfig position

data Config
  = GetConfig
  | BumpConfig !Int
