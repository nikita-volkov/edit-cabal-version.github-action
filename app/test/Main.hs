module Main where

import qualified Coalmine.NumericVersion as NumericVersion
import Coalmine.Prelude
import Coalmine.Tasty
import qualified EditCabalVersion as Editor

main :: IO ()
main =
  defaultMain . testGroup "Editor" $
    [ testGroup
        "Set Mode"
        [ eqTestCase
            "1"
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
        ],
      testGroup
        "Bump Mode"
        [ eqTestCase
            "New place"
            (Right "version: 0.2.1")
            (fmap Editor.modifiedResultText (Editor.bumpVersion 2 "version: 0.2")),
          eqTestCase
            "New place deep"
            (Right "version: 0.0.1")
            (fmap Editor.modifiedResultText (Editor.bumpVersion 2 "version: 0"))
        ]
    ]
