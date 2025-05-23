import Html

main :: IO ()
main = putStrLn $ render myHtml

myHtml :: Html
myHtml =
    html_ "My Page Title" (h1_ "This is the <BEST> Heading" <> p_ "Some paragraph" <> p_ "Some other paragraph")