module Main where

import qualified Coalmine.NumericVersion as NumericVersion
import Coalmine.Prelude
import Coalmine.Tasty
import qualified EditCabalVersion as Editor

main =
  defaultMain . testGroup "Editor" $
    [ eqTestCase
        "set"
        ( Right
            [i|
              cabal-version: 3.0

              name: test
              version: 0.2

              common common-settings
                default-language: Haskell2010
            |]
        )
        ( fmap
            Editor.modifiedResultText
            ( Editor.setVersion
                [NumericVersion.lit|0.2|]
                [i|
                cabal-version: 3.0

                name: test
                version: 0

                common common-settings
                  default-language: Haskell2010
              |]
            )
        )
    ]
