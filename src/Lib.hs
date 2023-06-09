module Lib (mainloop) where

import Control.Monad.State
import Lambda (rename)
import Parsers (parse)
import System.Console.ANSI
import Text.Megaparsec (errorBundlePretty)

mainloop :: String -> IO ()
mainloop "" = return ()
mainloop inp = do
  colored Blue "input" inp
  case parse inp of
    Left ex -> colored Red "error" (errorBundlePretty ex)
    Right e -> do
      colored Green "parse" (show e)
      let e' = evalState (rename e) (0, [])
      colored Yellow "renam" (show e')
      putChar '\n'
  where
    colored clr lbl txt = do
      setSGR [SetColor Foreground Vivid clr]
      putStr $ '[' : lbl ++ "] "
      setSGR [Reset]
      putStrLn txt
