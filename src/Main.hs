module Main where

import qualified Data.Map as M
import LieDetector

main :: IO()
main = do
    loop (M.fromList [("top", True), ("bottom", False)])

loop symTab = do
    str <- getLine
    if null str
    then return ()
    else let toks  = tokenise str
             tree  = parse toks
             Ev act = evaluate tree
             (result,symTab') = act symTab
         in do
             print result
             loop symTab' 

