module Converter exposing (..)

import Html exposing (Html, text, div, h1, p, span)
import Html.Attributes exposing (attribute, href, value, class, classList, type_, size)
import Textbox exposing (..)

-- MODEL

type alias Model = { a : TextboxModel, b : TextboxModel }

model : Model
model = { a = textboxModel, b = textboxModel }

getAccessors : Bool -> ( (Model -> TextboxModel), ((TextboxModel -> TextboxModel) -> Model -> Model) )
getAccessors which = case which of
  False -> ( .a, (\f model -> { model | a = f model.a }) )
  True -> ( .b, (\f model -> { model | b = f model.b }) )

-- UPDATE

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
        [ Html.map (Textbox False) <| textboxView model.a
        , span [ class "separator" ] [ text "=" ]
        , Html.map (Textbox True) <| textboxView model.b
        ]
      ]
    ]
