module Ui exposing (ColorPalette(..), FontSize(..), Spacing(..), color, fontsize, noEdges, space, tooltip)

import Color
import Element exposing (Element, el)
import Html.Attributes



{- Types -}


type ColorPalette
    = TextColor
    | PrimaryColor
    | DangerColor
    | InfoColor
    | WhiteColor
    | GreenColor
    | BlackColor
    | GrayColor
    | PurpleColor
    | YellowColor


type FontSize
    = TitleSize
    | TextSize


type Spacing
    = SmallSpace
    | RegularSpace
    | LargeSpace



{- Scales -}


scaled : Float -> Float -> Int -> Int
scaled base scale int =
    Element.modular base scale int
        |> round


scaledFontsize : Int -> Int
scaledFontsize =
    scaled 16 1.25


scaledSpacing : Int -> Int
scaledSpacing =
    scaled 8 2



{- Exposed utils -}


noEdges =
    { top = 0, left = 0, right = 0, bottom = 0 }


color : ColorPalette -> Element.Color
color palette =
    let
        { red, green, blue, alpha } =
            Color.toRgba <|
                case palette of
                    TextColor ->
                        Color.darkCharcoal

                    PrimaryColor ->
                        Color.lightBlue

                    DangerColor ->
                        Color.darkRed

                    InfoColor ->
                        Color.blue

                    WhiteColor ->
                        Color.white

                    GreenColor ->
                        Color.darkGreen

                    BlackColor ->
                        Color.black

                    GrayColor ->
                        Color.gray

                    PurpleColor ->
                        Color.purple

                    YellowColor ->
                        Color.yellow
    in
    Element.rgba red green blue alpha


fontsize : FontSize -> Int
fontsize size =
    case size of
        TitleSize ->
            scaledFontsize 4

        TextSize ->
            scaledFontsize 1


space : Spacing -> Int
space size =
    case size of
        LargeSpace ->
            scaledSpacing 3

        RegularSpace ->
            scaledSpacing 2

        SmallSpace ->
            scaledSpacing 1



{- Element -}


tooltip : (Element msg -> Element.Attribute msg) -> Element Never -> Element.Attribute msg
tooltip usher tooltip_ =
    Element.inFront <|
        el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.transparent True
            , Element.mouseOver [ Element.transparent False ]
            , (usher << Element.map never) <|
                el [ Element.htmlAttribute (Html.Attributes.style "pointerEvents" "none") ]
                    tooltip_
            ]
            Element.none
