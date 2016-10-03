{-# LANGUAGE OverloadedStrings #-}
import Turtle hiding (need, (</>))

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified Data.Text as T


latexmk file = shell ("latexmk -shell-escape -pdf " <> file) empty

withCwd :: MonadIO m => Turtle.FilePath -> m a -> m a
withCwd dir act = do
  cwd <- pwd
  cd dir
  r <- act
  cd cwd
  return r

main :: IO ()
main = shakeArgs shakeOptions $ do
    want [ "tex" </> "applying-fp.pdf" ]

    "//*.pdf" %> \out -> do
      let inp = out -<.> "tex"
          dir = fromString (takeDirectory inp)
      need [inp]
      void $ withCwd dir $ latexmk (T.pack (takeFileName inp))
