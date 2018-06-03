module Plot exposing (..)

import Html exposing (Html, div)
import Unit exposing (EstradiolUnit, convert)

-- MODEL

type alias Model = { width: Maybe Float, value : Maybe (Float, EstradiolUnit) }

model : Model
model = { width = Nothing, value = Nothing }

-- MESSAGE

type Message =
  WidthChanged Float | SetValue (Maybe (Float, EstradiolUnit))

update : Message -> Model -> Model
update msg model = case msg of
  WidthChanged width -> { model | width = Just width }
  SetValue value -> { model | value = value }

-- VIEW

view : Model -> Html Message
view model =
  div [] []
