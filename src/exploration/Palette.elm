module Palette exposing (..)

import Color as ElmColor
import Element exposing (Color, rgb, rgb255, rgba, rgba255)



-- SPACING


xxSmall : Int
xxSmall =
    4


xSmall : Int
xSmall =
    8


small : Int
small =
    16


smallNormal : Int
smallNormal =
    24


normal : Int
normal =
    32


large : Int
large =
    64


xLarge : Int
xLarge =
    128


xxLarge : Int
xxLarge =
    256



-- COLORS


white : Color
white =
    rgb 1 1 1


whiteAlpha : Float -> Color
whiteAlpha alpha =
    rgba 1 1 1 alpha


black : Color
black =
    rgb 0 0 0


blackAlpha : Float -> Color
blackAlpha alpha =
    rgba 0 0 0 alpha


freeSpeechBlue : Color
freeSpeechBlue =
    rgb255 60 64 198



-- HELP


toUiColor : ElmColor.Color -> Color
toUiColor color =
    color
        |> ElmColor.toRgba
        |> (\c -> rgba c.red c.green c.blue c.alpha)
