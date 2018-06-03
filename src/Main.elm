port module Main exposing (..)

import Html exposing (beginnerProgram)
import Converter

port viewWidths : (Float -> msg) -> Sub msg

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL

type alias Model = Converter.Model

init : (Model, Cmd Message)
init = (Converter.model, Cmd.none)

-- UPDATE

type alias Message = Converter.Message

update msg model = (Converter.update msg model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions model = viewWidths Converter.SetWidth

-- VIEW

view = Converter.view
