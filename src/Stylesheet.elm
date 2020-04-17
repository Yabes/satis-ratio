module Stylesheet exposing (ColorPalette(..), FontSize(..), Spacing(..), edges, stylesheetColor, stylesheetFontsize, stylesheetSpacing)

import Element



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
        TextColor ->
            tangoColor Dark Charcoal

        PrimaryColor ->
            tangoColor Light Blue

        DangerColor ->
            tangoColor Dark Red

        InfoColor ->
            tangoColor Regular Blue

        WhiteColor ->
            tangoColor Regular White

        GreenColor ->
            tangoColor Dark Green

        BlackColor ->
            tangoColor Regular Black

        GrayColor ->
            tangoColor Regular Gray


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



{- Tango Colors -}


type ColorLuminance
    = Light
    | Regular
    | Dark


type ColorHue
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple
    | Brown
    | Gray
    | Charcoal
    | Black
    | White


tangoColor : ColorLuminance -> ColorHue -> Element.Color
tangoColor luminance hue =
    case ( luminance, hue ) of
        ( Light, Red ) ->
            Element.rgb255 239 41 41

        ( Regular, Red ) ->
            Element.rgb255 204 0 0

        ( Dark, Red ) ->
            Element.rgb255 164 0 0

        ( Light, Orange ) ->
            Element.rgb255 252 175 62

        ( Regular, Orange ) ->
            Element.rgb255 245 121 0

        ( Dark, Orange ) ->
            Element.rgb255 206 92 0

        ( Light, Yellow ) ->
            Element.rgb255 255 233 79

        ( Regular, Yellow ) ->
            Element.rgb255 237 212 0

        ( Dark, Yellow ) ->
            Element.rgb255 196 160 0

        ( Light, Green ) ->
            Element.rgb255 138 226 52

        ( Regular, Green ) ->
            Element.rgb255 115 210 22

        ( Dark, Green ) ->
            Element.rgb255 78 154 6

        ( Light, Blue ) ->
            Element.rgb255 114 159 207

        ( Regular, Blue ) ->
            Element.rgb255 52 101 164

        ( Dark, Blue ) ->
            Element.rgb255 32 74 135

        ( Light, Purple ) ->
            Element.rgb255 173 127 168

        ( Regular, Purple ) ->
            Element.rgb255 117 80 123

        ( Dark, Purple ) ->
            Element.rgb255 92 53 102

        ( Light, Brown ) ->
            Element.rgb255 233 185 110

        ( Regular, Brown ) ->
            Element.rgb255 193 125 17

        ( Dark, Brown ) ->
            Element.rgb255 143 89 2

        ( _, Black ) ->
            Element.rgb255 0 0 0

        ( _, White ) ->
            Element.rgb255 255 255 255

        ( Light, Gray ) ->
            Element.rgb255 238 238 236

        ( Regular, Gray ) ->
            Element.rgb255 211 215 207

        ( Dark, Gray ) ->
            Element.rgb255 186 189 182

        ( Light, Charcoal ) ->
            Element.rgb255 136 138 133

        ( Regular, Charcoal ) ->
            Element.rgb255 85 87 83

        ( Dark, Charcoal ) ->
            Element.rgb255 46 52 54
