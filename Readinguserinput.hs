module ReadingUserinput where

import SyntaxTree
import Parser
import Eval

repl :: IO ()
repl = loop NormalOrder
  where
    loop strat = do
      putStr "λ> "
      line <- getLine
      case line of
        ":q" -> putStrLn "bye"
        ":cbv" -> putStrLn "call-by-value" >> loop CallByValue
        ":normal" -> putStrLn "normal-order" >> loop NormalOrder
        _ ->
          case parseExpr line of
            Left err -> print err >> loop strat
            Right e  -> print (eval strat 500 e) >> loop strat