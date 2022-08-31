module EditCabalVersion where

import Coalmine.Prelude hiding (parseVersion)

editFile :: Path -> (Text -> Either Text Text) -> IO ()
editFile =
  error "TODO"

parseVersion :: Text -> Either Text [Int]
parseVersion =
  error "TODO"

setVersion :: [Int] -> Text -> Text
setVersion =
  error "TODO"

renderVersion :: [Int] -> Text
renderVersion =
  error "TODO"

bumpVersionInText ::
  -- | Index of the bumped version section.
  -- Major, Minor, Patch and etc.
  Int ->
  Text ->
  Either Text Text
bumpVersionInText position text =
  parseVersion text
    <&> bumpVersion position
    <&> renderVersion

bumpVersion ::
  -- | Index of the bumped version section.
  -- Major, Minor, Patch and etc.
  Int ->
  [Int] ->
  [Int]
bumpVersion =
  error "TODO"
