module Main where

import           Development.Shake
import           Development.Shake.FilePath

main :: IO ()
main =
  shakeArgs shakeOptions {shakeThreads = 1
                         ,shakeTimings = False} $
  do want ["dist/style.css"
          ,"dist/index.html"]
     action requireStatic
     "dist/index.html" %>
       \out ->
         do files <-
              getDirectoryFiles ""
                                ["src//*.elm","src//*.js"]
            need files
            elmMake "src/App.elm" out
     "dist/style.css" %>
       \out ->
         do let baseCss = "static/styles/main.less"
            need [baseCss]
            getDirectoryFiles ""
                              ["static/styles//*.less"] >>=
              need
            cmd "lessc" baseCss out

requireStatic :: Action ()
requireStatic =
  getDirectoryFiles
    "static"
    ["//*.ico","//*.json","//*.js","//*.html","//*.png","//*.gif"] >>=
  needUnder "dist"

needUnder :: FilePath -> [FilePath] -> Action ()
needUnder dir files =
  need [dir </> x | x <- files]

elmMake root out =
  cmd "elm-make" root "--yes" "--warn" ["--output=" ++ out]
