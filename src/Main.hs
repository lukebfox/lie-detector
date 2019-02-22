module Main where

import qualified Data.Map as M
import LieDetector

main :: IO()
main = do
  loop (M.fromList[("top",True),("bottom",False)])


loop symTab = do
    str <- getLine
    if null str
        then return ()
        else let tokens = tokenise str
                 tree = parse tokens
                 (result, symTab') = evaluate tree symTab
              in do print result
                    loop symTab'
