module CLI.Internal where

import qualified Markup
import qualified Html
import Convert (convert)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

run :: IO ()
run =
    getArgs >>= \args ->
        case args of
            -- With input and output file paths as program arguments
            [input, output] ->
                readFile input >>= \content ->
                    doesFileExist output >>= \exists ->
                    let
                        writeResult = writeFile output (process input content)
                    in
                        if exists
                            then whenIO confirm writeResult
                            else writeResult

            -- Any other kind of program arguments
            _ ->
                putStrLn "Usage: runghc main.hs [-- <input-file> <output-file>]"

process :: String -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm =
    putStrLn "Are you sure? (y/n)" *>
        getLine >>= \answer ->
            case answer of
                "y" -> pure True
                "n" -> pure False
                _ -> putStrLn "Invalid response. use y or n" *> confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
    cond >>= \result ->
        if result
            then action
            else pure ()