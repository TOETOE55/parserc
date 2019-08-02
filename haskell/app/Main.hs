module Main where

import Parser
import Syntax

interaction :: IO ()
interaction = do
    putStrLn "please input the sentence:"
    str <- getLine
    let parseResult = evalParser sentence (initParseState str)
    case parseResult of
        Right sen -> do
            putStrLn "the analysis result is:"
            putStrLn (sentencePrettyPrint sen)
        Left (Err err) -> do
            putStrLn "failed to analysis: "
            putStrLn err

    putStrLn "\nagain..."
    interaction


main :: IO ()
main = do
    putStrLn "-- This is a symple English parser."
    putStrLn "-- It can only be used to analyze simple sentences.\n"
    interaction
