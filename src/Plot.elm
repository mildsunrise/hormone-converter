module Plot exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (fill, rx, ry, textAnchor, class, d, transform, stroke, strokeWidth, strokeLinecap, strokeLinejoin)
import Html exposing (Html)
import Html.Events exposing (on)
import Json.Encode
import Json.Decode
import Unit exposing (EstradiolUnit, convert, unitLabel, defaultUnit)
width = toString >> Svg.Attributes.width
height = toString >> Svg.Attributes.height
x = toString >> Svg.Attributes.x
y = toString >> Svg.Attributes.y

-- MODEL

type alias Model = { width: Maybe Float, value : Maybe (Float, EstradiolUnit) }

model : Model
model = { width = Just 400, value = Nothing }

-- MESSAGE

type Message =
  WidthChanged Float | SetValue (Maybe (Float, EstradiolUnit))

update : Message -> Model -> Model
update msg model = case msg of
  WidthChanged width -> { model | width = Just width }
  SetValue value -> { model | value = value }

-- VIEW

view : Model -> Html Message
view model = case model.width of
  Nothing -> Html.span [] []
  Just viewWidth -> actualView viewWidth model

actualView : Float -> Model -> Html Message
actualView viewWidth model = let

    unit = model.value |> Maybe.map Tuple.second |> Maybe.withDefault defaultUnit

    lowerBound = -5
    upperBound = 160
    mapW v = v / (upperBound - lowerBound) * (viewWidth-8)
    map v = mapW (v - lowerBound) + 4
    mapWI v = v  / (viewWidth-8) * (upperBound - lowerBound)
    mapI x = mapWI (x - 4) + lowerBound

    barRectX pstart pend start end attrs = let
        s = map start + pstart
        e = map end + pend
      in
        rect ([ x s, width (e-s), y 30, height 12, class "segment" ] ++ attrs) []
    barRect = barRectX 0 0

    rawSpacing = convert defaultUnit (mapWI 60) unit
    spacingRound = floor (logBase 10 rawSpacing) |> toFloat |> (^) 10
    spacing = rawSpacing / spacingRound |> round |> toFloat |> (*) spacingRound
      |> (\n -> convert unit n defaultUnit)
    firstLabel = (0) / spacing |> ceiling |> toFloat |> (*) spacing
    segmentCount = (mapI (viewWidth-0) - firstLabel) / spacing |> floor
    labels = List.range 0 segmentCount |> List.map (\n -> firstLabel + spacing * toFloat n)
    label pos = text_ [ x <| map pos, y 15, textAnchor "middle", fill "#fff" ]
      [ text <| toString <| convert defaultUnit pos unit ]

    valueIndicator pos =
      rect [ x (map pos - 2), width 4, y (26-4), height (4*2+20),
         rx "2", ry "2", fill "#fff", class "indicator value" ] []
    overflowIndicator =
      path [ translate (viewWidth - 16) 36, d "M -13,-13 L 0,0 L -13,13",
        stroke "#fff", strokeWidth "4", strokeLinecap "round",
        strokeLinejoin "round", fill "none", class "indicator overflow" ] []
    indicator pos = if pos > (mapI <| viewWidth - 10)
      then overflowIndicator else valueIndicator pos

  in svg [ height 75, width viewWidth ] <|
    [ g [ class "bar" ]
      [ rect [ x 0, width viewWidth, y (30-4), height 20,
          rx "10", ry "10", fill "#fff", class "bg" ] []
      , barRectX 0 6 lowerBound 40 [ fill "#c74900", rx "6", ry "6" ]
      , barRectX -6 0 110 upperBound [ fill "#00c700", rx "6", ry "6" ]
      , barRect 40 80 [ fill "#a3a800" ]
      , barRect 80 110 [ fill "#2ae600" ]
      ]
    , text_ [ x (viewWidth/2), y 68, textAnchor "middle",
        fill "#fff", class "unit-label" ] [ text <| unitLabel unit ]
    , g [ class "labels" ] <| List.map label labels
    , (case model.value of
       Nothing -> g [] []
       Just (value, unit) -> indicator (convert unit value defaultUnit))
    ]

translate x y =
  transform <| "translate(" ++ (toString x) ++ " " ++ (toString y) ++ ")"
