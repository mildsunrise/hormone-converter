import Html exposing (..)
import Html.Attributes exposing (attribute, href, value, class, classList, type_, size)
import Html.Events exposing (..)
import Regex exposing (regex, replace)
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


-- TEXT BOX

type alias TextboxModel =
  { text : String, value : Maybe Float, unit : Maybe EstradiolUnit, unitSet : Bool }

textboxModel : TextboxModel
textboxModel = { text = "", value = Nothing, unit = Nothing, unitSet = False }

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
    Maybe.withDefault (let
      unit = Maybe.map reverseUnit model.unit |> Maybe.withDefault defaultUnit
    in { model | unit = Just unit, unitSet = True })

  Submit ->
    -- Try to apply value
    trySetValue model |> Maybe.withDefault model

  SetValue value unit ->
    { text = formatNumber value, value = Just value,
      unit = Just unit, unitSet = True }

  SetUnit unit ->
    { model | unit = Just unit }

trySetValue : TextboxModel -> Maybe TextboxModel
trySetValue model =
  parseNumber model.text |> Maybe.andThen (\value -> let
    unit = Maybe.withDefault defaultUnit model.unit
  in Just { model | value = Just value, unit = Just unit, unitSet = True })

getValue : TextboxModel -> Maybe (Float, EstradiolUnit)
getValue model = Maybe.map2 (,) model.value model.unit

textboxView : TextboxModel -> Html TextboxMessage
textboxView model =
  div [ class "textbox" ]
    [ form [ onSubmit Submit ]
      [ input [ onInput Input, value model.text, type_ "text", size 5,
                attribute "inputmode" "numeric", attribute "pattern" " *[0-9.,e-]+ *" ] []
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


-- CONVERTER

type alias Model = { a : TextboxModel, b : TextboxModel }

model : Model
model = { a = textboxModel, b = textboxModel }

getAccessors : Bool -> ( (Model -> TextboxModel), ((TextboxModel -> TextboxModel) -> Model -> Model) )
getAccessors which = case which of
  False -> ( .a, (\f model -> { model | a = f model.a }) )
  True -> ( .b, (\f model -> { model | b = f model.b }) )

type Message =
  Textbox Bool TextboxMessage

update : Message -> Model -> Model
update msg model = case msg of
  Textbox which msg ->
    let
      (this, mapThis) = getAccessors which
      (other, mapOther) = getAccessors (not which)
    in
      -- Forward message to corresponding textbox
      mapThis (textboxUpdate msg) model |>
      -- Then, if textbox has a value, set it (converted) on the other
      (\model -> case getValue (this model) of
        Just (value, unit) -> let
            (value2, unit2) = calculateEquivalence value unit
          in mapOther (textboxUpdate <| SetValue value2 unit2) model
        Nothing -> model ) |>
      -- If we're displaying same units on both textboxes, switch the other one
      (\model -> case Maybe.map2 (,) (this model).unit (other model).unit of
        Just (unit, _) ->
          mapOther (textboxUpdate <| SetUnit <| reverseUnit unit) model
        Nothing -> model )

view : Model -> Html Message
view model =
  div [ Html.Attributes.id "container" ]
    [ div [ class "converter" ]
      [ h1 [] [ text "Conversor de estradiol" ]
      , p [] [ text """
Introduce la cantidad que quieras convertir en la caja de abajo.
        """ ]
      , div [ class "boxes" ]
        [ Html.map (Textbox False) <| textboxView model.a
        , span [ class "separator" ] [ text "=" ]
        , Html.map (Textbox True) <| textboxView model.b
        ]
      ]
    ]

main = Html.beginnerProgram { model = model, view = view, update = update }


-- UTILS

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
