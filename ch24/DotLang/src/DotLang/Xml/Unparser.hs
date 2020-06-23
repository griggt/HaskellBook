module DotLang.Xml.Unparser
  ( unparseElement
  ) where

import Data.Char (ord)
import Data.List (intercalate)

import DotLang.Xml.Parser

------------------------------------------------------------------------
unparseElement :: Element -> String
unparseElement = element
------------------------------------------------------------------------

element :: Element -> String
element (Element stag attrs body mEtag) = case mEtag of
  Just etag -> head ++ '>' : inner ++ "</" ++ etag ++ ">"
  Nothing   -> head ++ "/>"
  where
    head = '<' : stag ++ (if null att then "" else " " ++ att)
    inner = concatMap content body
    att = intercalate " " $ map attribute attrs

content :: Content -> String
content (ContentCharData x) = charData x
content (ContentElement x) = element x
content (ContentReference x) = reference x
content (ContentComment x) = comment x

reference :: Reference -> String
reference (CharRef c) = "&#" ++ (show $ ord c) ++ ";"
reference (EntityRef n) = '&' : (name n) ++ ";"

charData :: CharData -> String
charData = id

name :: Name -> String
name = id

attrValue :: AttrValue -> String
attrValue (AttrValue xs q) = [q] ++ (concatMap f xs) ++ [q]
  where
    f (AChar c) = [c]
    f (ARef r) = reference r

attribute :: Attribute -> String
attribute (Attribute name val) = name ++ "=" ++ attrValue val

comment :: Comment -> String
comment (Comment x) = "<!--" ++ x ++ "-->"
