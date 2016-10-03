{-# LANGUAGE OverloadedStrings #-}
import Turtle hiding (need, (</>))

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified Data.Text as T

main :: IO ()
main = shakeArgs shakeOptions $ do
    want [ "tex" </> "applying-fp.pdf" ]

    "//*.pdf" %> \out -> do
      let inp = out -<.> "tex"
      need [inp]
      void $ do
        let dir = fromString (takeDirectory inp) :: Turtle.FilePath
        echo (T.pack $ takeDirectory inp)
        cd dir
        dir <- pwd
        echo (format fp dir)
        shell ("latexmk -pdf " <> T.pack (takeFileName inp)) empty
        cd ".."
