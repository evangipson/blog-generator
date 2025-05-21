-- INTERNAL EXPORT
module Html.Internal where

-- TYPES
newtype Html = Html String
newtype Structure = Structure String

-- ACTIONS
render :: Html -> String
render html =
    case html of
        Html str -> str

-- EMBEDDED DOMAIN-SPECIFIC LANGUAGE (EDSL)
html_ :: String -> Structure -> Html
html_ title content =
    Html $
        el "html" $
            el "head" (el "title" (escape title)) <>
                el "body" (getStructureString content)

p_ :: String -> Structure = Structure . el "p" . escape

h1_ :: String -> Structure = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure = list_ "ul"

ol_ :: [Structure] -> Structure = list_ "ol"

code_ :: String -> Structure = Structure . el "pre" . escape

append_ :: Structure -> Structure -> Structure
append_ a b = Structure $ getStructureString a <> getStructureString b

-- HELPERS
list_ :: String -> [Structure] -> Structure
list_ tag = Structure . el tag . concatMap
    (el "li" . getStructureString)

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString struct = case struct of Structure str -> str

escape :: String -> String =
    let
        escapeChar c = case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '&' -> "&amp;"
            '"' -> "&quot;"
            '\'' -> "&#39;"
            _ -> [c]
    in
        concatMap escapeChar