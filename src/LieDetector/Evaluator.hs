module LieDetector.Evaluator (
  evaluate
  ) where

import LieDetector.Lexer
import LieDetector.Parser
import qualified Data.Map as M

-- persistant data structure for storing variables
type SymTab = M.Map String Bool

lookUp :: String -> SymTab -> (Bool, SymTab)
lookUp str symTab =
    case M.lookup str symTab of
        Just val -> (val, symTab)
        Nothing -> error $ "Undefined variable: " ++ str

addSymbol :: String -> Bool -> SymTab -> ((), SymTab)
addSymbol key value symTab =
    let symTab' = M.insert key value symTab
     in ((), symTab')

--threads symbol table through evaluations
evaluate :: Tree -> SymTab -> (Bool, SymTab)
evaluate (JunctionNode op lst rst) symTab =
    let (l, symTab') = evaluate lst symTab
        (r, symTab'') = evaluate rst symTab'
     in case op of
            And -> (l && r, symTab'')
            Or -> (l || r, symTab'')
evaluate (ConditionalNode op lst rst) symTab =
    let (l, symTab') = evaluate lst symTab
        (r, symTab'') = evaluate rst symTab'
     in case op of
            LImply -> (l || (not r), symTab'')
            RImply -> ((not l) || r, symTab'')
evaluate (NotNode tree) symTab =
    let (t, symTab') = evaluate tree symTab
     in (not t, symTab')
evaluate (AssignNode str tree) symTab =
  let (val, symTab') = evaluate tree symTab
      ((),symTab'') = addSymbol str val symTab'
      in (val, symTab'')
evaluate (VarNode name) symTab = lookUp name symTab
evaluate (BoolNode bool) symTab = (bool, symTab)

