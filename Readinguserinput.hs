module ReadingUserinput where
 
import SyntaxTree
import Parser
import Eval
import Substitution (subst, freeVars)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.List (isPrefixOf)
import System.IO
 
type Env = Map String Expr

preludeSource :: [(String, String)]
preludeSource =
  [ ("id",    "\\x. x")
  , ("true",  "\\t f. t")
  , ("false", "\\t f. f")
  , ("0",     "\\f x. x")
  , ("1",     "\\f x. f x")
  , ("2",     "\\f x. f (f x)")
  , ("3",     "\\f x. f (f (f x))")
  , ("4",     "\\f x. f (f (f (f x)))")
  , ("succ",  "\\n f x. f (n f x)")
  , ("plus",  "\\m n f x. m f (n f x)")
  , ("mult",  "\\m n f. m (n f)")
  ]
 
loadPrelude :: Env
loadPrelude = foldr add Map.empty preludeSource
  where
    add (name, src) env = case parseExpr src of
      Right e -> Map.insert name e env
      Left _  -> env  
 

resolve :: Env -> Expr -> Expr
resolve env = go (500 :: Int)
  where
    go 0 e = e
    go n e =
      case [v | v <- S.toList (freeVars e), Map.member v env] of
        []      -> e
        (v : _) -> go (n - 1) (subst v (env Map.! v) e)
 

traceReduction :: Strategy -> Int -> Expr -> [Expr]
traceReduction strat limit e0 = e0 : go limit e0
  where
    go 0 _   = []
    go n cur = case step strat cur of
      Nothing  -> []
      Just cur' -> cur' : go (n - 1) cur'
 
repl :: IO ()
repl = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  loop NormalOrder loadPrelude
  where
    loop strat env = do
      putStr "ʎ> "
      hFlush stdout
      line <- getLine
      case line of
        ":q" -> putStrLn "bye"
        ":cbv"    -> putStrLn "call-by-value" >> loop CallByValue env
        ":normal" -> putStrLn "normal-order"  >> loop NormalOrder env
        ":env" -> do
          mapM_ (\(k, v) -> putStrLn (k ++ " = " ++ show v)) (Map.toList env)
          loop strat env
        _ | ":let " `isPrefixOf` line ->
              case break (== '=') (drop 5 line) of
                (name, '=' : rhs) ->
                  case parseExpr rhs of
                    Left err -> print err >> loop strat env
                    Right e  -> do
                      let name' = trim name
                      putStrLn (name' ++ " defined")
                      loop strat (Map.insert name' e env)
                _ -> putStrLn "usage: :let name = expr" >> loop strat env
          | ":trace " `isPrefixOf` line ->
              case parseExpr (drop 7 line) of
                Left err -> print err >> loop strat env
                Right e  -> do
                  mapM_ print (traceReduction strat 500 (resolve env e))
                  loop strat env
        _ ->
          case parseExpr line of
            Left err -> print err >> loop strat env
            Right e  -> print (eval strat 500 (resolve env e)) >> loop strat env
 
    trim = f . f
      where f = reverse . dropWhile (== ' ')