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
      !NumericVersion
      !Text
      -- ^ Suffix.

instance LenientParser CabalContents where
  lenientParser = do
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

instance CompactPrinting CabalContents where
  toCompactBuilder (CabalContents a b c) =
    to a <> toCompactBuilder b <> to c

traverseCabalContentsVersion :: Functor f => (NumericVersion -> f NumericVersion) -> CabalContents -> f CabalContents
traverseCabalContentsVersion mapper (CabalContents a b c) =
  mapper b <&> \b -> CabalContents a b c

-- * Final

bumpVersionInText ::
  -- | Index of the bumped version section.
  -- Major, Minor, Patch and etc.
  Int ->
  Text ->
  Either Text Text
bumpVersionInText position text = do
  contents <- parse lenientParser text
  case traverseCabalContentsVersion (NumericVersion.bump position) contents of
    Nothing -> Left "Missing position"
    Just contents -> Right $ printCompactAs contents
