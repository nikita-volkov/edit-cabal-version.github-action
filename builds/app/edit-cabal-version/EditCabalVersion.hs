module EditCabalVersion
  ( bumpVersionInText,
  )
where

import Coalmine.Prelude hiding (parseVersion)

parseVersion :: Text -> Either Text [Int]
parseVersion =
  error "TODO"

setVersion :: [Int] -> Text -> Text
setVersion =
  error "TODO"

renderVersion :: [Int] -> Text
renderVersion =
  error "TODO"

bumpVersion ::
  -- | Index of the bumped version section.
  -- Major, Minor, Patch and etc.
  Int ->
  [Int] ->
  [Int]
bumpVersion =
  error "TODO"

-- * Final

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
