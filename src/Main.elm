import Html exposing (beginnerProgram)
import Converter exposing (model, view, update)

main = Html.beginnerProgram { model = model, view = view, update = update }
