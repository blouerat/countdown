module Main

import Parsers

maxDepth : Nat
maxDepth = 42

onInput : String -> String
onInput input = case run maxDepth expression input of
                     Left err => "Huh, nope: " ++ show err ++ "\n"
                     Right exp => "Nice one: " ++ show exp ++ "\n"

main : IO ()
main = repl "Enter a valid countdown number expression: " onInput
