module Converter exposing (..)

import Html exposing (Html, text, div, h1, p, span)
import Html.Attributes exposing (attribute, href, value, class, classList, type_, size)
import Textbox exposing (..)

-- MODEL

type alias Model = { a : Textbox.Model, b : Textbox.Model }

model : Model
model = { a = Textbox.model, b = Textbox.model }

getAccessors : Bool -> ( (Model -> Textbox.Model), ((Textbox.Model -> Textbox.Model) -> Model -> Model) )
getAccessors which = case which of
  False -> ( .a, (\f model -> { model | a = f model.a }) )
  True -> ( .b, (\f model -> { model | b = f model.b }) )

-- UPDATE

type Message =
  Textbox Bool Textbox.Message

update : Message -> Model -> Model
update msg model = case msg of
  Textbox which msg -> updateTextbox which msg model

updateTextbox : Bool -> Textbox.Message -> Model -> Model
updateTextbox which msg model = let
    (this, mapThis) = getAccessors which
    (other, mapOther) = getAccessors (not which)
  in
    -- Forward message to corresponding textbox
    mapThis (Textbox.update msg) model |>
    -- Then, if textbox has a value, set it (converted) on the other and plot
    (\model -> case getValue (this model) of
      Just (value, unit) -> let
          unit2 = reverseUnit unit
          value2 = convert unit value unit2
        in mapOther (Textbox.update <| SetValue value2 unit2) model
      Nothing -> model ) |>
    -- If we're displaying same units on both textboxes, switch the other one
    (\model -> case Maybe.map2 (,) (this model).unit (other model).unit of
      Just (unit, _) ->
        mapOther (Textbox.update <| SetUnit <| reverseUnit unit) model
      Nothing -> model )

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
        [ Html.map (Textbox False) <| Textbox.view model.a
        , span [ class "separator" ] [ text "=" ]
        , Html.map (Textbox True) <| Textbox.view model.b
        ]
      ]
    ]
