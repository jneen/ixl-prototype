import Ixl (parseIxl)
import System.Console.Readline (readline, addHistory)
import System.Posix (queryTerminal)

makeRepl :: (String -> IO ()) -> String -> IO ()
makeRepl interp prompt = do
  line <- readline prompt
  case line of
       Nothing -> putStrLn ""
       Just str -> do
         addHistory str
         interp str
         makeRepl interp prompt

repl :: String -> IO ()
repl = makeRepl $ \input -> do
  case parseIxl "(repl)" input of
       Left error -> print error
       Right tree -> print tree -- evalIxl tree >>= print

-- runInput = do
--   c <- getContents
--   case parseIxl "(stdin)" c of
--        Left e -> do putStrLn "Error parsing input:"
--                     print e
--        Right r -> do
--          print r
--          return ()

main = repl ".> "
--   isatty <- queryTerminal 0
--   case isatty of
--        True -> repl ".> "
--        False -> runInput
