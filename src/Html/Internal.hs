-- INTERNAL EXPORT
module Html.Internal where

-- IMPORTS
import Numeric.Natural

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

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

ul_ :: [Structure] -> Structure
ul_ = list_ "ul"

ol_ :: [Structure] -> Structure
ol_ = list_ "ol"

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

instance Semigroup Structure where
    (<>) :: Structure -> Structure -> Structure
    c1 <> c2 = Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
    mempty = Structure ""

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