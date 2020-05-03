module Satis.Energy exposing (EnergyGenerator, MW, add, getNeededGenerator, ioPerGenerator, make, mul, powerPerGenerator, viewGenerator, viewMW)

import Element exposing (Element, el, text)
import Element.Font as Font
import Satis.IO exposing (ItemIO, perMinute)
import Satis.Product exposing (IntermediateProduct(..))
import Ui



{- types -}


type EnergyGenerator
    = BiomassBurner
    | CoalGenerator
    | FuelGenerator


type MW
    = MW Int



{- utils -}


make : Int -> MW
make int =
    MW int


mul : Int -> MW -> MW
mul coef (MW a) =
    MW (a * coef)


add : MW -> MW -> MW
add (MW a) (MW b) =
    MW (a + b)


powerPerGenerator : EnergyGenerator -> MW
powerPerGenerator generator =
    case generator of
        BiomassBurner ->
            MW 30

        CoalGenerator ->
            MW 75

        FuelGenerator ->
            MW 150


getNeededGenerator : MW -> List ( EnergyGenerator, Int )
getNeededGenerator (MW power) =
    let
        generatorCount generator =
            let
                (MW perGenerator) =
                    powerPerGenerator generator
            in
            toFloat power
                / toFloat perGenerator
                |> ceiling
    in
    [ ( BiomassBurner, generatorCount BiomassBurner )
    , ( CoalGenerator, generatorCount CoalGenerator )
    ]


ioPerGenerator : EnergyGenerator -> List ItemIO
ioPerGenerator generator =
    case generator of
        BiomassBurner ->
            []

        CoalGenerator ->
            [ perMinute 15 Coal
            , perMinute 45 Water
            ]

        FuelGenerator ->
            [ perMinute 15 Oil ]



{- view -}


viewMW : MW -> Element msg
viewMW (MW a) =
    let
        innerText =
            String.fromInt a ++ "MW"
    in
    el [ Font.bold, Font.color <| Ui.color Ui.YellowColor ] (text innerText)


viewGenerator : EnergyGenerator -> Element msg
viewGenerator generator =
    case generator of
        BiomassBurner ->
            el
                [ Font.color (Ui.color Ui.GreenColor)
                , Font.bold
                ]
                (text "Biomass Burner")

        CoalGenerator ->
            el
                [ Font.color (Ui.color Ui.BlackColor)
                , Font.bold
                ]
                (text "Coal Generator")

        FuelGenerator ->
            el
                [ Font.color (Ui.color Ui.PurpleColor)
                , Font.bold
                ]
                (text "Fuel Generator")
