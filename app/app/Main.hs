import qualified Coalmine.HappyPathIO as HappyPathIO
import Coalmine.Prelude
import EditCabalVersion

main :: IO ()
main = do
  path <- findCabalFile
  position <- readPosition
  editFile path (bumpVersionInText position)

editFile :: Path -> (Text -> Either Text Text) -> IO ()
editFile =
  error "TODO"

findCabalFile :: IO Path
findCabalFile =
  error "TODO"

readPosition :: IO Int
readPosition =
  error "TODO"
