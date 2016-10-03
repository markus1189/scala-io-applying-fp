{-# LANGUAGE OverloadedStrings #-}
import Turtle hiding (need, (</>))

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified Data.Text as T

main :: IO ()
main = shakeArgs shakeOptions $ do
    want [ "applying-fp.pdf" ]

    "*.pdf" %> \out -> do
      need ["tex" </> out -<.> "tex"]
      void $ do
        cd "tex"
        shell ("latexmk -pdf " <> T.pack out) empty
