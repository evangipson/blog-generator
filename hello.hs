import Html

main :: IO ()
main = putStrLn $ render myHtml

myHtml :: Html
myHtml =
    html_ "My Page Title" $
        append_ (h1_ "This is the <BEST> Heading") $
        append_ (p_ "Paragraph #1 & some other info") (p_ "Paragraph #2")