import qualified Coalmine.EvenSimplerPaths as Path
import qualified Coalmine.HappyPathIO as HappyPathIO
import Coalmine.Prelude
import qualified Data.Text.IO as TextIO
import EditCabalVersion

main :: IO ()
main = do
  path <- findCabalFile
  position <- readArgs
  editFile path (bumpVersionInText position)

editFile :: Path -> (Text -> Either Text Text) -> IO ()
editFile path editor = do
  content <- TextIO.readFile pathString
  content <- case editor content of
    Right content -> return content
    Left err -> die [i|Failed to edit file "$path": $err|]
  TextIO.writeFile pathString content
  where
    pathString = Path.toString path

findCabalFile :: IO Path
findCabalFile = do
  files <- filter ((==) ["cabal"] . Path.extensions) <$> Path.listDirectory "."
  case files of
    [file] -> return file
    [] -> die "No Cabal-file found"
    _ -> die "More than one Cabal-file found"

readArgs :: IO Int
readArgs =
  getArgs >>= \case
    [a] -> case readMaybe a of
      Just a -> return a
      Nothing -> die $ "Invalid position: " <> a
    _ -> die "Invalid amount of args. Expecting 1"
