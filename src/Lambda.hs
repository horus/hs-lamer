module Lambda (Expr (..), rename) where

import Control.Monad.State
import Data.Bifunctor (second)

data Expr
  = Var String
  | Abs String Expr
  | App Expr Expr

--  deriving (Show)

instance Show Expr where
  show (Var s) = s
  show (App a b) = '(' : show a ++ " " ++ show b ++ ")"
  show (Abs s e) = "(\955" ++ s ++ "." ++ show e ++ ")"

type Binding = (String, String)

type Env = (Int, [Binding])

-- alpha convension
rename :: Expr -> State Env Expr
rename (Var s) = maybe (Var <$> tag s) (return . Var) . lookup s =<< gets snd
rename (App e1 e2) = liftM2 App (rename e1) (rename e2)
rename (Abs capt e) = do
  tagged <- tag capt
  e' <- rename e
  untag capt
  return $ Abs tagged e'

tag :: String -> State Env String
tag s = do
  (i, bindings) <- get
  let t = '$' : s ++ show i
  put (i + 1, (s, t) : bindings)
  return t

untag :: String -> State Env ()
untag s = modify $ second $ \xs ->
  -- any freevar will add itself to the list, don't blindly use tail
  let (h, t) = break ((== s) . fst) xs
   in h ++ drop 1 t
