module Utils exposing (..)

import Regex exposing (regex, replace)

parseNumber : String -> Maybe Float
parseNumber =
  String.trim >> replace Regex.All (regex ",|'") (\_ -> ".") >>
  String.toFloat >> Result.toMaybe

formatNumber : Float -> String
formatNumber = (*) 100 >> round >> toString >> String.padLeft 3 '0' >>
  (\s -> String.dropRight 2 s ++ "." ++ String.right 2 s)

fallback : Maybe a -> Maybe a -> Maybe a
fallback base new = case new of
  Nothing -> base
  Just _ -> new

filterNegative : comparable -> Maybe comparable
filterNegative n = if n < 0 then Nothing else Just n
