import Numeric.Natural

-- TYPES
type Document = [Structure]
data Structure =
    Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]

example1 :: Document
example1 = [Paragraph "Hello, world!"]

example2 :: Document
example2 = [UnorderedList ["Welcome"],
    Paragraph "To this tutorial about Haskell."]

example3 :: Document
example3 = [Paragraph "Remember that multiple lines with no separation are grouped together into a single paragraph but list items remain separate.",
    OrderedList ["Item 1 of a list", "Item 2 of a list"]]

example4 :: Document
example4 = [Heading 1 "Compiling programs with ghc",
    Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries.",
    Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:",
    CodeBlock ["main = putStrLn \"Hello, Haskell!\""],
    Paragraph "Now, we can compile the program by invoking ghc with the file name:",
    CodeBlock ["➜ ghc hello.hs",
        "[1 of 1] Compiling Main             ( hello.hs, hello.o )",
        "Linking hello..."],
    Paragraph "GHC created the following files:",
    UnorderedList ["hello.hi - Haskell interface file",
        "hello.o - Object file, the output of the compiler before linking",
        "hello (or hello.exe on Microsoft Windows) - A native runnable executable."],
    Paragraph "GHC will produce an executable when the source file satisfies both conditions:",
    OrderedList ["Defines the main function in the source file",
        "Defines the module name to be Main or does not have a module declaration"],
    Paragraph "Otherwise, it will only produce the .o and .hi files."]

parse :: String -> Document
parse = parseLines [] . lines -- (1)

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph)) -- (2), (3)
  in
    case txts of -- (4)
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest -- (5)
          else
            parseLines (currentLine : currentParagraph) rest -- (6)

trim :: String -> String
trim = unwords . words