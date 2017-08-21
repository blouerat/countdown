module Main

import Parsers

displayError : (error : Error) -> (input : String) -> String
displayError (MkError _ column message) input =
    "Nope!\n" ++
    input ++ "\n" ++
    pack (replicate (cast column) ' ') ++ "^ " ++ message ++ "\n"

onInput : String -> String
onInput input = case run input of
                     Right expression => "Nice one: " ++ show expression ++ "\n"
                     Left error => displayError error input

main : IO ()
main = repl "Enter a valid countdown number expression: " onInput
