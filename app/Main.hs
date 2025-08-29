import qualified Data.Map as Map
import Eval (eval)
import Parser (parseMutilExpr)
import Syntax (Env, Expr)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  let expr = "def f x => if x < 2 then x else f (x - 1) + f (x - 2); f 10;"
      env = Map.empty :: Env

  case parseMutilExpr expr of
    Right ast -> do
      putStrLn $
        "AST: " ++ show ast
      evaluateAndPrint env ast
    Left err -> do
      putStrLn $ errorBundlePretty err

evaluateAndPrint :: Env -> [Expr] -> IO ()
evaluateAndPrint _ [] = putStrLn "Finished."
evaluateAndPrint currentEnv (ast : asts) = do
  let (result, newEnv) = eval currentEnv ast
  putStrLn $ "Expression: " ++ show ast
  putStrLn $ "Result: " ++ show result
  putStrLn "---"
  evaluateAndPrint newEnv asts
