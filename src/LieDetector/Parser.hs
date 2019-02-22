module LieDetector.Parser (
  Tree(..),
  parse
              ) where
import LieDetector.Lexer

--represents a parse tree
data Tree = JunctionNode Operator Tree Tree
          | ConditionalNode Operator Tree Tree
          | AssignNode String Tree
          | NotNode Tree
          | VarNode String
          | BoolNode Bool
    deriving Show

--top level function making the initial call to
--expression and returning the complete parse tree
parse :: [Token] -> Tree
parse toks
    | null toks' = tree
    | otherwise = error $ "Leftover tokens: " ++ show toks
  where
    (tree, toks') = expression toks


--consumes a token and returns a node in the parse tree with unused tokens 
expression :: [Token] -> (Tree, [Token])
expression toks =
    case lookAhead toks' of
        (TokOp op)
            | elem op [LImply, RImply] ->
                (ConditionalNode op termTree exTree, toks'')
        TokAssign ->
            case termTree of
                VarNode name -> (AssignNode name exTree, toks'')
                _ -> error "Only variables can be assigned to."
        _ -> (termTree, toks')
  where
    (termTree, toks') = term toks
    (exTree, toks'') = expression $ tail toks'


--consumes a token and returns a node in the parse tree with unused tokens 
term :: [Token] -> (Tree, [Token])
term toks =
    case lookAhead toks' of
        (TokOp op)
            | elem op [And, Or] -> (JunctionNode op junctTree termTree, toks'')
        _ -> (junctTree, toks')
  where
    (junctTree, toks') = junctor toks
    (termTree, toks'') = term $ tail toks'


--consumes a token and returns a node in the parse tree with unused tokens 
junctor :: [Token] -> (Tree, [Token])
junctor toks =
    case lookAhead toks of
        (TokBool bool) -> (BoolNode bool, tail toks)
        (TokIdent name) -> (VarNode name, tail toks)
        TokNot ->
            let (junctTree, toks') = junctor $ tail toks
             in (NotNode junctTree, toks')
        TokLParen ->
            let (expTree, toks') = expression $ tail toks
             in if lookAhead toks' /= TokRParen
                    then error "Missing right parenthesis."
                    else (expTree, tail toks')
        _ -> error $ "Parse error on token: " ++ show toks
