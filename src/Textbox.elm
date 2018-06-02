module Textbox exposing (..)

import Html exposing (Html, text, form, input, div, button)
import Html.Attributes exposing (attribute, href, value, class, classList, type_, size)
import Html.Events exposing (onInput, onSubmit, onClick)
import Maybe exposing (andThen, withDefault)
import Utils exposing (fallback, parseNumber, formatNumber)
(=>) = (,)

-- ESTRADIOL

type EstradiolUnit = PmolL | PgMl

guessUnit : String -> Maybe EstradiolUnit
guessUnit text = let
    guess n = if n < 0 || n > 1000 then Nothing else
      Just (if n < 130 then PgMl else PmolL)
  in parseNumber text |> Maybe.andThen guess

reverseUnit : EstradiolUnit -> EstradiolUnit
reverseUnit unit = case unit of
  PmolL -> PgMl
  PgMl -> PmolL

calculateEquivalence : Float -> EstradiolUnit -> (Float, EstradiolUnit)
calculateEquivalence n unit = case unit of
  PmolL -> (n / 3.671, PgMl)
  PgMl -> (n * 3.671, PmolL)

defaultUnit = PgMl

-- MODEL

type alias TextboxModel =
  { text : String, value : Maybe Float, unit : Maybe EstradiolUnit, unitSet : Bool }

textboxModel : TextboxModel
textboxModel = { text = "", value = Nothing, unit = Nothing, unitSet = False }

-- UPDATE

type TextboxMessage =
  -- HTML messages
  Submit | Input String | UnitAction |
  -- External control messages
  SetValue Float EstradiolUnit | SetUnit EstradiolUnit

textboxUpdate : TextboxMessage -> TextboxModel -> TextboxModel
textboxUpdate msg model = case msg of

  Input text ->
    -- Remove any stored value, store new text and guess unit if needed
    { model | text = text, value = Nothing } |>
    (\model -> if model.unitSet then model else
      { model | unit = guessUnit text |> fallback model.unit })

  UnitAction ->
    -- Try to apply value if not set already
    (if model.value == Nothing then trySetValue model else Nothing) |>
    -- If we didn't apply, just switch units
    withDefault (let
      unit = Maybe.map reverseUnit model.unit |> withDefault defaultUnit
    in { model | unit = Just unit, unitSet = True })

  Submit ->
    -- Try to apply value
    trySetValue model |> withDefault model

  SetValue value unit ->
    { text = formatNumber value, value = Just value,
      unit = Just unit, unitSet = True }

  SetUnit unit ->
    { model | unit = Just unit }

-- EXTERNAL METHODS

trySetValue : TextboxModel -> Maybe TextboxModel
trySetValue model =
  parseNumber model.text |> andThen (\value -> let
    unit = withDefault defaultUnit model.unit
  in Just { model | value = Just value, unit = Just unit, unitSet = True })

getValue : TextboxModel -> Maybe (Float, EstradiolUnit)
getValue model = Maybe.map2 (,) model.value model.unit

-- VIEW

textboxView : TextboxModel -> Html TextboxMessage
textboxView model =
  div [ class "textbox" ]
    [ form [ onSubmit Submit ]
      [ input [ onInput Input, value model.text, type_ "text", size 5,
                attribute "inputmode" "numeric",
                attribute "pattern" " *[0-9.,e-]+ *" ] []
      ]
    , unitButton model
    ]

unitButton : TextboxModel -> Html TextboxMessage
unitButton model = let
    label = case model.unit of
      Just unit -> case unit of
        PmolL -> "pmol/L"
        PgMl -> "pg/mL"
      Nothing -> ""
    className = if model.unit == Nothing then "empty" else
      if model.unitSet then "set" else "guessed"
  in button [ onClick UnitAction,
    classList ["unit" => True, className => True] ] [ text label ]
