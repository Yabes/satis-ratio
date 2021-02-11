module Satis.Recipe exposing (Recipe, assemblyMachineText, byproduct, consumption, getRecipeOf, getRecipesUsingResidue, input, order, output, outputWithByroduct, viewTooltip)

import Element exposing (Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Option exposing (Options)
import Satis.Energy as Energy exposing (MW)
import Satis.IO as IO exposing (ItemIO)
import Satis.Options.AlternateRecipe exposing (AltRecipe(..))
import Satis.Product exposing (IntermediateProduct(..))
import Ui


type Recipe
    = -- Extractor
      Miner ItemIO
    | WaterExtractor ItemIO
    | OilExtractor ItemIO
      -- Furnaces
    | Smelter { input_ : ItemIO, output_ : ItemIO }
    | Foundry { input_ : ( ItemIO, ItemIO ), output_ : ItemIO }
      -- Transformation
    | Constructor { input_ : ItemIO, output_ : ItemIO }
    | Assembler { input_ : ( ItemIO, ItemIO ), output_ : ItemIO }
    | Refinery { input_ : ( ItemIO, Maybe ItemIO ), output_ : ItemIO, byproduct_ : Maybe ItemIO }
    | Residue ItemIO



{- utils -}


input : Recipe -> List ItemIO
input recipe =
    let
        tupleToList tuple =
            let
                ( a, b ) =
                    tuple
            in
            [ a, b ]

        optionalTupleToList tuple =
            case tuple of
                ( a, Nothing ) ->
                    [ a ]

                ( a, Just b ) ->
                    [ a, b ]
    in
    case recipe of
        Miner _ ->
            []

        WaterExtractor _ ->
            []

        OilExtractor _ ->
            []

        Smelter { input_ } ->
            [ input_ ]

        Foundry { input_ } ->
            tupleToList input_

        Constructor { input_ } ->
            [ input_ ]

        Assembler { input_ } ->
            tupleToList input_

        Refinery { input_ } ->
            optionalTupleToList input_

        Residue _ ->
            []


output : Recipe -> ItemIO
output recipe =
    case recipe of
        Miner output_ ->
            output_

        WaterExtractor output_ ->
            output_

        OilExtractor output_ ->
            output_

        Smelter { output_ } ->
            output_

        Foundry { output_ } ->
            output_

        Constructor { output_ } ->
            output_

        Assembler { output_ } ->
            output_

        Refinery { output_ } ->
            output_

        Residue output_ ->
            output_


byproduct : Recipe -> Maybe ItemIO
byproduct recipe =
    case recipe of
        Refinery { byproduct_ } ->
            byproduct_

        _ ->
            Nothing


outputWithByroduct : Recipe -> List ItemIO
outputWithByroduct recipe =
    let
        output_ =
            output recipe

        optionalByproduct =
            byproduct recipe
    in
    case optionalByproduct of
        Nothing ->
            [ output_ ]

        Just byproduct_ ->
            [ output_, byproduct_ ]


consumption : Recipe -> MW
consumption recipe =
    case recipe of
        Assembler _ ->
            Energy.make 15

        Constructor _ ->
            Energy.make 4

        Smelter _ ->
            Energy.make 12

        WaterExtractor _ ->
            Energy.make 20

        Miner _ ->
            Energy.make 5

        Foundry _ ->
            Energy.make 16

        Refinery _ ->
            Energy.make 30

        OilExtractor _ ->
            Energy.make 40

        Residue _ ->
            Energy.make 0



{- recipe finder -}


getRecipeOf : Options AltRecipe -> IntermediateProduct -> Recipe
getRecipeOf options product =
    case product of
        IronOre ->
            Miner <| IO.perMinute 60 IronOre

        CopperOre ->
            Miner <| IO.perMinute 60 CopperOre

        CateriumOre ->
            Miner <| IO.perMinute 60 CateriumOre

        Coal ->
            Miner <| IO.perMinute 60 Coal

        Water ->
            WaterExtractor <| IO.perMinute 120 Water

        LimeStone ->
            Miner <| IO.perMinute 60 LimeStone

        Concrete ->
            Constructor
                { input_ = IO.perMinute 45 LimeStone
                , output_ = IO.perMinute 15 Concrete
                }

        CateriumIngot ->
            Smelter
                { input_ = IO.perMinute 45 CateriumOre
                , output_ = IO.perMinute 15 CateriumIngot
                }

        Quickwire ->
            Constructor
                { input_ = IO.perMinute 12 CateriumIngot
                , output_ = IO.perMinute 60 Quickwire
                }

        CopperWire ->
            if Option.isActivated FusedWire options then
                Assembler
                    { input_ = ( IO.perMinute 70 CopperIngot, IO.perMinute 7.5 CateriumIngot )
                    , output_ = IO.perMinute 225 CopperWire
                    }

            else
                Constructor
                    { input_ = IO.perMinute 15 CopperIngot
                    , output_ = IO.perMinute 30 CopperWire
                    }

        CopperSheet ->
            if Option.isActivated SteamedCopperSheet options then
                Refinery
                    { input_ = ( IO.perMinute 27.5 Water, Just <| IO.perMinute 27.5 CopperIngot )
                    , output_ = IO.perMinute 27.5 CopperSheet
                    , byproduct_ = Nothing
                    }

            else
                Constructor
                    { input_ = IO.perMinute 20 CopperIngot
                    , output_ = IO.perMinute 10 CopperSheet
                    }

        CopperCable ->
            Constructor
                { input_ = IO.perMinute 60 CopperWire
                , output_ = IO.perMinute 30 CopperCable
                }

        IronPlate ->
            Constructor
                { input_ = IO.perMinute 30 IronIngot
                , output_ = IO.perMinute 20 IronPlate
                }

        IronRod ->
            Constructor
                { input_ = IO.perMinute 15 IronIngot
                , output_ = IO.perMinute 15 IronRod
                }

        Screw ->
            Constructor
                { input_ = IO.perMinute 10 IronRod
                , output_ = IO.perMinute 40 Screw
                }

        ReinforcedIronPlate ->
            Assembler
                { input_ = ( IO.perMinute 30 IronPlate, IO.perMinute 60 Screw )
                , output_ = IO.perMinute 5 ReinforcedIronPlate
                }

        Rotor ->
            if Option.isActivated CopperRotor options then
                Assembler
                    { input_ = ( IO.perMinute 56.25 CopperSheet, IO.perMinute 487.5 Screw )
                    , output_ = IO.perMinute 28.1 Rotor
                    }

            else
                Assembler
                    { input_ = ( IO.perMinute 20 IronRod, IO.perMinute 100 Screw )
                    , output_ = IO.perMinute 4 Rotor
                    }

        SmartPlating ->
            Assembler
                { input_ = ( IO.perMinute 2 ReinforcedIronPlate, IO.perMinute 2 Rotor )
                , output_ = IO.perMinute 2 SmartPlating
                }

        ModularFrame ->
            if Option.isActivated BoltedModularFrame options then
                Assembler
                    { input_ = ( IO.perMinute 7.5 ReinforcedIronPlate, IO.perMinute 140 Screw )
                    , output_ = IO.perMinute 5 ModularFrame
                    }

            else
                Assembler
                    { input_ = ( IO.perMinute 3 ReinforcedIronPlate, IO.perMinute 12 IronRod )
                    , output_ = IO.perMinute 2 ModularFrame
                    }

        Steel ->
            Foundry
                { input_ = ( IO.perMinute 45 IronOre, IO.perMinute 45 Coal )
                , output_ = IO.perMinute 45 Steel
                }

        SteelPipe ->
            Constructor
                { input_ = IO.perMinute 30 Steel
                , output_ = IO.perMinute 20 SteelPipe
                }

        SteelBeam ->
            Constructor
                { input_ = IO.perMinute 60 Steel
                , output_ = IO.perMinute 15 SteelBeam
                }

        IndustrialSteelBeam ->
            Assembler
                { input_ = ( IO.perMinute 24 Steel, IO.perMinute 30 Concrete )
                , output_ = IO.perMinute 6 IndustrialSteelBeam
                }

        VersatileFramework ->
            Assembler
                { input_ = ( IO.perMinute 2.5 ModularFrame, IO.perMinute 30 SteelBeam )
                , output_ = IO.perMinute 5 VersatileFramework
                }

        AutomaticWire ->
            Assembler
                { input_ = ( IO.perMinute 2.5 Stator, IO.perMinute 50 CopperWire )
                , output_ = IO.perMinute 2.5 AutomaticWire
                }

        Stator ->
            if Option.isActivated QuickwireStator options then
                Assembler
                    { input_ = ( IO.perMinute 40 SteelPipe, IO.perMinute 150 Quickwire )
                    , output_ = IO.perMinute 20 Stator
                    }

            else
                Assembler
                    { input_ = ( IO.perMinute 15 SteelPipe, IO.perMinute 8 CopperWire )
                    , output_ = IO.perMinute 5 Stator
                    }

        Motor ->
            Assembler
                { input_ = ( IO.perMinute 10 Rotor, IO.perMinute 10 Stator )
                , output_ = IO.perMinute 5 Motor
                }

        IronIngot ->
            Smelter
                { input_ = IO.perMinute 30 IronOre
                , output_ = IO.perMinute 30 IronIngot
                }

        CopperIngot ->
            Smelter
                { input_ = IO.perMinute 30 CopperOre
                , output_ = IO.perMinute 30 CopperIngot
                }

        Oil ->
            OilExtractor <| IO.perMinute 120 Oil

        Fuel ->
            Refinery
                { input_ = ( IO.perMinute 60 Oil, Nothing )
                , output_ = IO.perMinute 40 Fuel
                , byproduct_ = Just (IO.perMinute 30 PolymerResidue)
                }

        Plastic ->
            Refinery
                { input_ = ( IO.perMinute 30 Oil, Nothing )
                , output_ = IO.perMinute 20 Plastic
                , byproduct_ = Just (IO.perMinute 10 HeavyOilResidue)
                }

        Rubber ->
            Refinery
                { input_ = ( IO.perMinute 30 Oil, Nothing )
                , output_ = IO.perMinute 20 Rubber
                , byproduct_ = Just (IO.perMinute 20 HeavyOilResidue)
                }

        Coke ->
            Residue <| IO.perMinute 0 Coke

        HeavyOilResidue ->
            Residue <| IO.perMinute 0 HeavyOilResidue

        PolymerResidue ->
            Residue <| IO.perMinute 0 PolymerResidue


getRecipesUsingResidue : IntermediateProduct -> List Recipe
getRecipesUsingResidue product =
    case product of
        PolymerResidue ->
            [ Refinery
                { input_ = ( IO.perMinute 60 PolymerResidue, Just <| IO.perMinute 20 Water )
                , output_ = IO.perMinute 20 Plastic
                , byproduct_ = Nothing
                }
            , Refinery
                { input_ = ( IO.perMinute 40 PolymerResidue, Just <| IO.perMinute 40 Water )
                , output_ = IO.perMinute 20 Rubber
                , byproduct_ = Nothing
                }
            ]

        HeavyOilResidue ->
            [ Refinery
                { input_ = ( IO.perMinute 60 HeavyOilResidue, Nothing )
                , output_ = IO.perMinute 40 Fuel
                , byproduct_ = Nothing
                }
            , Refinery
                { input_ = ( IO.perMinute 40 HeavyOilResidue, Nothing )
                , output_ = IO.perMinute 120 Coke
                , byproduct_ = Nothing
                }
            ]

        _ ->
            []


order : Recipe -> Int
order recipe =
    case recipe of
        Miner _ ->
            10

        WaterExtractor _ ->
            11

        OilExtractor _ ->
            12

        Smelter _ ->
            20

        Foundry _ ->
            21

        Refinery _ ->
            22

        Constructor _ ->
            30

        Assembler _ ->
            31

        Residue _ ->
            99



{- view -}


assemblyMachineText : Recipe -> String
assemblyMachineText recipe =
    case recipe of
        Constructor _ ->
            "Constructor"

        Assembler _ ->
            "Assembler"

        Miner _ ->
            "Miner"

        WaterExtractor _ ->
            "Pump"

        Smelter _ ->
            "Smelter"

        Foundry _ ->
            "Foundry"

        Refinery _ ->
            "Refinery"

        OilExtractor _ ->
            "Oil Extractor"

        Residue _ ->
            "Residue"


viewTooltip : Recipe -> Element msg
viewTooltip recipe =
    let
        ioColumn : List ItemIO -> Element msg
        ioColumn list =
            column [] (List.map IO.view list)
    in
    column
        [ Background.color <| Ui.color Ui.WhiteColor
        , Border.color <| Ui.color Ui.BlackColor
        , Border.rounded 5
        , Border.width 1
        , Element.padding <| Ui.space Ui.SmallSpace
        , Element.spacing <| Ui.space Ui.RegularSpace
        ]
        [ row []
            [ el [] (text "per machine:")
            , el [ Element.alignRight ] (Energy.viewMW <| consumption recipe)
            ]
        , row [ Element.spacing <| Ui.space Ui.LargeSpace ]
            [ ioColumn <| input recipe
            , el [ Element.centerY ] (text "→")
            , ioColumn <| outputWithByroduct recipe
            ]
        ]
