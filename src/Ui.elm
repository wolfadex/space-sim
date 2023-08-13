module Ui exposing (Transformation(..), alignSelf, backgroundColor, borderColor, borderRadius, borderStyle, borderWidth, column, el, fontColor, fontSize, fontUnderline, gap, height, justifySelf, link, map, noAttribute, none, padding, paragraph, row, rowWrapped, stack, text, transform, translate, width)

import Html exposing (Html)
import Html.Attributes
import Ui.Theme



-- ELEMENTS


none : Html msg
none =
    Html.text ""


text : String -> Html msg
text =
    Html.text


el : List (Html.Attribute msg) -> Html msg -> Html msg
el attributes child =
    Html.div
        (Html.Attributes.class "wolfadex-el" :: attributes)
        [ child ]


column : List (Html.Attribute msg) -> List (Html msg) -> Html msg
column attributes children =
    Html.div
        (Html.Attributes.class "wolfadex-column" :: attributes)
        children


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attributes children =
    Html.div
        (Html.Attributes.class "wolfadex-row" :: attributes)
        children


rowWrapped : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rowWrapped attributes children =
    Html.div
        (Html.Attributes.class "wolfadex-row-wrapped" :: attributes)
        children


stack : List (Html.Attribute msg) -> List (Html msg) -> Html msg
stack attributes children =
    Html.div
        (Html.Attributes.class "wolfadex-stacked" :: attributes)
        children


paragraph : List (Html.Attribute msg) -> List (Html msg) -> Html msg
paragraph attributes children =
    Html.p
        (Html.Attributes.style "width" "100%" :: attributes)
        children



-- STYLES


padding :
    { rem1 : Html.Attribute msg
    , remHalf : Html.Attribute msg
    , remQuarter : Html.Attribute msg
    , xy : { rem1 : { remHalf : Html.Attribute msg } }
    }
padding =
    { rem1 = Html.Attributes.style "padding" "1rem"
    , remHalf = Html.Attributes.style "padding" "0.5rem"
    , remQuarter = Html.Attributes.style "padding" "0.25rem"
    , xy =
        { rem1 =
            { remHalf = Html.Attributes.style "padding" "0.5rem 1rem"
            }
        }
    }


height :
    { fill : Html.Attribute msg
    , shrink : Html.Attribute msg
    }
height =
    { fill = Html.Attributes.style "height" "100%"
    , shrink = Html.Attributes.style "height" "fit-content"
    }


width : { shrink : Html.Attribute msg }
width =
    { shrink = Html.Attributes.style "width" "fit-content" }


gap :
    { remQuarter : Html.Attribute msg
    , remHalf : Html.Attribute msg
    , rem1 : Html.Attribute msg
    , rem3 : Html.Attribute msg
    }
gap =
    { remQuarter = Html.Attributes.style "gap" "0.25rem"
    , remHalf = Html.Attributes.style "gap" "0.5rem"
    , rem1 = Html.Attributes.style "gap" "1rem"
    , rem3 = Html.Attributes.style "gap" "3rem"
    }


justifySelf :
    { center : Html.Attribute msg
    , end : Html.Attribute msg
    }
justifySelf =
    { center = Html.Attributes.style "justify-self" "center"
    , end = Html.Attributes.style "justify-self" "end"
    }


alignSelf : { center : Html.Attribute msg }
alignSelf =
    { center = Html.Attributes.style "align-self" "center" }


fontSize :
    { rem1 : Html.Attribute msg
    , rem2 : Html.Attribute msg
    , rem3 : Html.Attribute msg
    }
fontSize =
    { rem1 = Html.Attributes.style "font-size" "1rem"
    , rem2 = Html.Attributes.style "font-size" "2rem"
    , rem3 = Html.Attributes.style "font-size" "3rem"
    }


fontColor : Ui.Theme.Color -> Html.Attribute msg
fontColor color =
    Html.Attributes.style "color" (Ui.Theme.toCss color)


borderRadius :
    { remHalf : Html.Attribute msg
    , remQuarter : Html.Attribute msg
    , px3 : Html.Attribute msg
    }
borderRadius =
    { remHalf = Html.Attributes.style "border-radius" "0.5rem"
    , remQuarter = Html.Attributes.style "border-radius" "0.25rem"
    , px3 = Html.Attributes.style "border-radius" "3px"
    }


borderStyle : { solid : Html.Attribute msg }
borderStyle =
    { solid = Html.Attributes.style "border-style" "solid" }


borderWidth :
    { px1 : Html.Attribute msg
    , px2 : Html.Attribute msg
    , px3 : Html.Attribute msg
    , remHalf : Html.Attribute msg
    , bottom : { px1 : Html.Attribute msg }
    }
borderWidth =
    { px1 = Html.Attributes.style "border-width" "1px"
    , px2 = Html.Attributes.style "border-width" "2px"
    , px3 = Html.Attributes.style "border-width" "3px"
    , remHalf = Html.Attributes.style "border-width" "0.5rem"
    , bottom =
        { px1 = Html.Attributes.style "border-width" "0 0 1px 0"
        }
    }


borderColor : Ui.Theme.Color -> Html.Attribute msg
borderColor color =
    Html.Attributes.style "border-color" (Ui.Theme.toCss color)


backgroundColor : Ui.Theme.Color -> Html.Attribute msg
backgroundColor color =
    Html.Attributes.style "background-color" (Ui.Theme.toCss color)


noAttribute : Html.Attribute msg
noAttribute =
    Html.Attributes.class ""


fontUnderline : Html.Attribute msg
fontUnderline =
    Html.Attributes.style "text-decoration" "underline"


link =
    { external =
        \attributes options ->
            Html.a
                (attributes
                    ++ [ Html.Attributes.href options.url
                       , Html.Attributes.target "_blank"
                       ]
                )
                [ options.label
                ]
    }


map : (a -> b) -> Html a -> Html b
map =
    Html.map


translate :
    { down : Float -> Transformation
    , left : Float -> Transformation
    }
translate =
    { down = TranslateY
    , left = negate >> TranslateX
    }


type Transformation
    = TranslateX Float
    | TranslateY Float


transform : List Transformation -> Html.Attribute msg
transform transformations =
    Html.Attributes.style
        "transform"
        (transformations
            |> List.map
                (\transformation ->
                    case transformation of
                        TranslateX distance ->
                            "translateX(" ++ String.fromFloat distance ++ "px)"

                        TranslateY distance ->
                            "translateY(" ++ String.fromFloat distance ++ "px)"
                )
            |> String.join " "
        )
