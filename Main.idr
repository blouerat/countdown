module Main

import Parsers

displayError : (error : Error) -> (input : String) -> String
displayError (MkError pos message) input =
    "Nope!\n" ++
    input ++ "\n" ++
    pack (replicate (cast pos) ' ') ++ "^ " ++ message ++ "\n"

onInput : String -> String
onInput input = case run input of
                     Right (expression, result) => "Nice one: " ++ show expression ++ " = " ++ show result ++ "\n"
                     Left error => displayError error input

main : IO ()
main = repl "Enter a valid countdown number expression: " onInput
