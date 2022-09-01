module EditCabalVersion
  ( bumpVersionInText,
  )
where

import qualified Coalmine.NumericVersion as NumericVersion
import Coalmine.Prelude hiding (parseVersion)
import qualified Data.Attoparsec.Text as Ap
import qualified Data.Text as Text

data CabalContents
  = CabalContents
      !Text
      -- ^ Prefix.
      !NumericVersion.NumericVersion
      !Text
      -- ^ Suffix.

parseCabal :: Text -> Either Text CabalContents
parseCabal =
  parse parser
  where
    parser =
      CabalContents <$> prefix <*> lenientParser <*> Ap.takeText
      where
        prefix = do
          (a, b) <- reverseManyTillPreserving prefixPart versionLinePrefix
          return . mconcat . reverse $ b <> a
          where
            prefixPart =
              (<>) <$> Ap.takeWhile (/= '\n') <*> (Text.singleton <$> Ap.char '\n')
            versionLinePrefix = do
              a <- Ap.asciiCI "version"
              b <- Ap.takeWhile isSpace
              c <- Ap.char ':'
              d <- Ap.takeWhile isSpace
              return $ [d, Text.singleton c, b, a]

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
