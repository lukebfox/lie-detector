module LieDetector.Evaluator (
  evaluate,
  Evaluator(..)
  ) where

import Prelude
import LieDetector.Lexer
import LieDetector.Parser
import Control.Applicative
import Control.Monad
import qualified Data.Map as M

-- state monad definition with function currying
newtype Evaluator a = Ev (SymTab -> (a,SymTab))
instance Functor Evaluator where
  fmap = liftM
instance Applicative Evaluator where
  pure = return 
  (<*>) = ap
instance Monad Evaluator where
  return x = Ev (\symTab -> (x,symTab))
  (Ev act) >>= k = Ev  $ \symTab ->
    let (x,symTab') = act symTab
        (Ev act') = k x
     in
        act' symTab'

-- persistant data structure for storing variables
type SymTab = M.Map String Bool

--evaluate threads symbol table through evaluations
evaluate :: Tree ->  Evaluator Bool
evaluate (JunctionNode op lst rst) = do
  l <- evaluate lst
  r <- evaluate rst
  case op of
    And -> return $ l && r
    Or -> return $ l || r
evaluate (ConditionalNode op lst rst) = do
  l <- evaluate lst
  r <- evaluate rst
  case op of
    LImply -> return $ l || (not r)
    RImply -> return $ (not l) || r
evaluate (NotNode tree) = do
    x <- evaluate tree
    return $ not x
evaluate (AssignNode str tree) = do
  x <- evaluate tree
  addSymbol str x
evaluate (VarNode name) = lookUp name 
evaluate (BoolNode bool) = return bool

--returns a function which takes a symbol table
--and looks up the supplied key in it,
--returning the value / table pair
lookUp :: String -> Evaluator Bool
lookUp str = Ev $ \symTab ->
    case M.lookup str symTab of
        Just val -> (val,symTab)
        Nothing -> error $ "Undefined variable: " ++ str
        
--returns a function which takes a symbol table,
--adds the supplied key and value to it,
--returning the value / table pair          
addSymbol :: String -> Bool -> Evaluator Bool
addSymbol key value = Ev $ \symTab ->
    let symTab' = M.insert key value symTab
     in (value,symTab')

