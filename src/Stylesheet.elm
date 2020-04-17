module Stylesheet exposing (ColorPalette(..), FontSize(..), Spacing(..), edges, stylesheetColor, stylesheetFontsize, stylesheetSpacing)

import Element



{- Types -}


type ColorPalette
    = PrimaryColor
    | DangerColor
    | InfoColor
    | WhiteColor
    | GreenColor
    | BlackColor


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


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


stylesheetColor : ColorPalette -> Element.Color
stylesheetColor palette =
    case palette of
        PrimaryColor ->
            Element.rgb255 41 60 75

        DangerColor ->
            Element.rgb255 204 0 0

        InfoColor ->
            Element.rgb255 52 101 164

        WhiteColor ->
            Element.rgb255 255 255 255

        GreenColor ->
            Element.rgb255 78 154 6

        BlackColor ->
            Element.rgb255 46 52 54


stylesheetFontsize : FontSize -> Int
stylesheetFontsize size =
    case size of
        TitleSize ->
            scaledFontsize 4

        TextSize ->
            scaledFontsize 1


stylesheetSpacing : Spacing -> Int
stylesheetSpacing size =
    case size of
        LargeSpace ->
            scaledSpacing 3

        RegularSpace ->
            scaledSpacing 2

        SmallSpace ->
            scaledSpacing 1
