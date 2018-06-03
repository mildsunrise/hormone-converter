module Unit exposing (..)

import Utils exposing (parseNumber)

type EstradiolUnit = PmolL | PgMl

unitMultiplier : EstradiolUnit -> Float
unitMultiplier unit = case unit of
  PmolL -> 3.671
  PgMl -> 1

unitLabel : EstradiolUnit -> String
unitLabel unit = case unit of
  PmolL -> "pmol/L"
  PgMl -> "pg/mL"

reverseUnit : EstradiolUnit -> EstradiolUnit
reverseUnit unit = case unit of
  PmolL -> PgMl
  PgMl -> PmolL

guessUnit : String -> Maybe EstradiolUnit
guessUnit text = let
    guess n = if n < 0 || n > 1000 then Nothing else
      Just (if n < 130 then PgMl else PmolL)
  in parseNumber text |> Maybe.andThen guess

convert : EstradiolUnit -> Float -> EstradiolUnit -> Float
convert unit n tunit = n / (unitMultiplier unit) * (unitMultiplier tunit)

defaultUnit = PgMl
