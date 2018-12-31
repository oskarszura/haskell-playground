module Main (
    main
) where

import Data.List (intercalate)
import SimpleJSON

main = printJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])

printJValue :: JValue -> IO()
printJValue v = putStrLn (renderJValue v)

renderJValue :: JValue -> String
renderJValue JNull          = "null"
renderJValue (JBool False)  = "false"
renderJValue (JBool True)   = "true"
renderJValue (JNumber n)    = show n
renderJValue (JString s)    = show s
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k, v) = show k ++ ": " ++ renderJValue v
          renderJValue (JArray a) = "[" ++ values a ++ "]"
            where values [] = ""
                  values vs = intercalate ", " (map renderJValue vs)
renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJValue vs)