module Converter exposing (..)

import Html exposing (Html, text, div, h1, p, span, a)
import Html.Attributes exposing (attribute, href, value, class, classList, style, type_, size)
import Html.Events exposing (onClick)
import Utils exposing (formatNumber)
import Unit exposing (EstradiolUnit, convert, reverseUnit, unitLabel)
import Textbox exposing (getValue, Message(SetValue, SetUnit))
import Plot
(=>) = (,)

-- MODEL

type alias Model = {
  textbox : Textbox.Model,
  value : Maybe (Float, EstradiolUnit),
  targetUnit : Maybe EstradiolUnit,
  width : Maybe Float }

model : Model
model = { textbox = Textbox.model, value = Nothing, targetUnit = Nothing, width = Nothing }

-- UPDATE

type Message =
  Textbox Textbox.Message | Reverse | SetWidth Float

update : Message -> Model -> Model
update msg model = case msg of
  Textbox msg ->
    -- Forward message to textbox
    { model | textbox = Textbox.update msg model.textbox }
    -- If a value was set, update value and targetUnit
    |> (\model -> case getValue model.textbox of
      Just (value, unit) ->
        { model | value = Just (value, unit), targetUnit = Just (reverseUnit unit) }
      Nothing -> model
    )
  Reverse ->
    -- Set textbox and value to the converted value, and targetUnit to original
    convertedValue model |> Maybe.map (\(value, unit) ->
      { model | textbox = Textbox.update (SetValue value unit) model.textbox,
        value = Just (value, unit),
        targetUnit = model.value |> Maybe.map Tuple.second }
    ) |>
    -- If no converted value was set, do nothing
    Maybe.withDefault model
  SetWidth width -> { model | width = Just width }

convertedValue : Model -> Maybe (Float, EstradiolUnit)
convertedValue model = let
    f (value, unit) unit2 = (convert unit value unit2, unit2)
  in Maybe.map2 f model.value model.targetUnit

-- VIEW

view : Model -> Html Message
view model =
  div [ Html.Attributes.id "container" ]
    [ div [ class "converter" ]
      [ h1 [] [ text "Conversor de estradiol" ]
      , p [] [ text """
Introduce la cantidad que quieras convertir en la caja de abajo.
        """ ]
      , div [ class "boxes" ]
        [ Html.map Textbox <| Textbox.view model.textbox
        , outputSeparator model
        , outputView model
        ]
      , div [ class "plot" ] [ Plot.view model.width <| convertedValue model ]
      ]
    ]

outputSeparator : Model -> Html Message
outputSeparator model = let
    visibility = if convertedValue model == Nothing then "hidden" else "visible"
    styles = [ "visibility" => visibility ]
  in a [ class "separator", onClick Reverse, style styles ] [ text "⇆" ]

outputView : Model -> Html Message
outputView model = let
    value = case convertedValue model of
      Just (value, unit) -> formatNumber value
      Nothing -> ""
    unit = model.targetUnit |> Maybe.map unitLabel |> Maybe.withDefault ""
  in div [ class "output" ]
    [ span []
      [ span [ class "value" ] [ text value ]
      , text " "
      , span [ class "unit" ] [ text unit ]
      ]
    ]
