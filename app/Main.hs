{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception
import Control.Monad (forever, when)
import Data.Char (isSpace)
import Lib (mainloop)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hIsTerminalDevice, hSetBuffering, stdin, stdout)

main :: IO ()
main =
  getArgs >>= \case
    [] -> do
      putStrLn "Hint: hit Ctrl-D to exit, both \\ and λ are ok\n"
      isTerm <- hIsTerminalDevice stdin
      hSetBuffering stdout NoBuffering
      forever (when isTerm (putStr "> ") >> getLine >>= mainloop . dropWhile isSpace)
        `catch` (\SomeException {} -> exitSuccess)
    ("--help" : _) -> do
      name <- getProgName
      putStrLn "horusl's alpha renamer\n"
      mapM_
        putStrLn
        [ name ++ " file1 [file2, ...]: parse content in file(s)",
          name ++ ": interactive, reads input from stdin",
          name ++ " --help: display this message"
        ]
    files ->
      let loop f = readFile f >>= mapM_ mainloop . lines
       in mapM_ loop files
