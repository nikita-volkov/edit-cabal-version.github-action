module EditCabalVersion
  ( bumpVersion,
    VersionBumped (..),
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

cabalContentsVersion :: CabalContents -> NumericVersion
cabalContentsVersion (CabalContents _ b _) = b

cabalContentsText :: CabalContents -> Text
cabalContentsText = printCompactAs

traverseCabalContentsVersion :: Functor f => (NumericVersion -> f NumericVersion) -> CabalContents -> f CabalContents
traverseCabalContentsVersion mapper (CabalContents a b c) =
  mapper b <&> \b -> CabalContents a b c

-- * Final

data VersionBumped = VersionBumped
  { versionBumpedOldVersion :: NumericVersion,
    versionBumpedNewVersion :: NumericVersion,
    versionBumpedText :: Text
  }

bumpVersion ::
  -- | Index of the bumped version section.
  -- Major, Minor, Patch and etc.
  Int ->
  Text ->
  Either Text VersionBumped
bumpVersion position text = do
  contents <- parse lenientParser text
  case traverseCabalContentsVersion onVersion contents of
    (oldVersion, contents) ->
      Right $
        VersionBumped
          oldVersion
          (cabalContentsVersion contents)
          (cabalContentsText contents)
  where
    onVersion version =
      case NumericVersion.bump position version of
        bumped -> (version, bumped)
