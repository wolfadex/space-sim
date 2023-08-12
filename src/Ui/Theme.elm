module Ui.Theme exposing
    ( Color
    , darkGray
    , error
    , green
    , nearlyWhite
    , nearlyWhiteTransparent
    , toCss
    )


type Color
    = Color { r : Float, g : Float, b : Float, a : Float }


toCss : Color -> String
toCss (Color { r, g, b, a }) =
    "rgba("
        ++ String.fromFloat (r * 255)
        ++ ","
        ++ String.fromFloat (g * 255)
        ++ ","
        ++ String.fromFloat (b * 255)
        ++ ","
        ++ String.fromFloat a
        ++ ")"


green : Color
green =
    Color { r = 0.4, g = 0.9, b = 0.7, a = 1 }


darkGray : Color
darkGray =
    Color { r = 0.2, g = 0.2, b = 0.2, a = 1 }


nearlyWhite : Color
nearlyWhite =
    Color { r = 0.9, g = 0.9, b = 0.9, a = 1 }


nearlyWhiteTransparent : Color
nearlyWhiteTransparent =
    Color { r = 0.9, g = 0.9, b = 0.9, a = 0.9 }


error : Color
error =
    Color { r = 1, g = 0.1, b = 0.1, a = 1 }
