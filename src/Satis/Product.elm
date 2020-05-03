module Satis.Product exposing (IntermediateProduct(..), all, color, parse, text)

import Color exposing (Color)
import Utils exposing (listFind)



{- Types -}


type
    IntermediateProduct
    -- Raw
    = IronOre
    | CopperOre
    | Coal
    | Water
    | LimeStone
    | CateriumOre
      -- Copper
    | CopperIngot
    | CopperWire
    | CopperCable
    | CopperSheet
      -- Iron
    | IronIngot
    | IronPlate
    | IronRod
    | Screw
    | ReinforcedIronPlate
      -- Caterium
    | CateriumIngot
    | Quickwire
      -- LimeStone
    | Concrete
      -- Advanced Tier1-2
    | Rotor
    | ModularFrame
    | SmartPlating
      -- Steel
    | Steel
    | SteelBeam
    | SteelPipe
    | IndustrialSteelBeam
      -- Advanced Tier3-4
    | Stator
    | Motor
    | VersatileFramework
    | AutomaticWire
      -- Oil procecing
    | Oil
    | Fuel
    | Coke
    | Plastic
    | Rubber
    | HeavyOilResidue
    | PolymerResidue


all : List IntermediateProduct
all =
    [ CopperWire, CopperCable, CopperSheet, IronPlate, IronRod, Screw, ReinforcedIronPlate, Rotor, ModularFrame, SmartPlating, Steel, SteelBeam, SteelPipe, IndustrialSteelBeam, Stator, Motor, VersatileFramework, AutomaticWire, Plastic, Rubber, Fuel ]



{- IntermediateProduct utils -}


color : IntermediateProduct -> Color
color product =
    case product of
        IronOre ->
            Color.rgb255 132 132 158

        CopperOre ->
            Color.rgb255 159 95 79

        CateriumOre ->
            Color.rgb255 164 144 101

        Coal ->
            Color.rgb255 64 64 77

        Water ->
            Color.rgb255 35 88 122

        LimeStone ->
            Color.rgb255 151 148 159

        Concrete ->
            Color.rgb255 197 187 142

        CateriumIngot ->
            Color.rgb255 172 165 138

        IronIngot ->
            Color.rgb255 143 145 135

        CopperIngot ->
            Color.rgb255 130 90 74

        Quickwire ->
            Color.rgb255 181 172 123

        CopperWire ->
            Color.rgb255 160 104 68

        CopperSheet ->
            Color.rgb255 173 129 109

        CopperCable ->
            Color.rgb255 115 117 115

        IronPlate ->
            Color.rgb255 167 168 174

        IronRod ->
            Color.rgb255 95 93 94

        Screw ->
            Color.rgb255 51 60 173

        ReinforcedIronPlate ->
            Color.rgb255 88 92 121

        Rotor ->
            Color.rgb255 110 61 36

        SmartPlating ->
            Color.rgb255 145 62 53

        ModularFrame ->
            Color.rgb255 129 130 130

        Steel ->
            Color.rgb255 47 52 64

        SteelPipe ->
            Color.rgb255 58 54 54

        SteelBeam ->
            Color.rgb255 39 39 46

        IndustrialSteelBeam ->
            Color.rgb255 234 225 186

        VersatileFramework ->
            Color.rgb255 144 61 45

        AutomaticWire ->
            Color.rgb255 165 64 42

        Stator ->
            Color.rgb255 103 115 160

        Motor ->
            Color.rgb255 165 135 54

        Oil ->
            Color.rgb255 18 17 19

        Fuel ->
            Color.rgb255 189 120 42

        Plastic ->
            Color.rgb255 101 169 242

        Rubber ->
            Color.rgb255 61 60 61

        Coke ->
            Color.rgb255 45 43 56

        HeavyOilResidue ->
            Color.rgb255 111 0 125

        PolymerResidue ->
            Color.rgb255 40 0 141


text : IntermediateProduct -> String
text product =
    case product of
        CopperIngot ->
            "Copper Ingot"

        CopperWire ->
            "Copper Wire"

        CopperCable ->
            "Copper Cable"

        CopperSheet ->
            "Copper Sheet"

        IronIngot ->
            "Iron Ingot"

        IronPlate ->
            "Iron Plate"

        IronRod ->
            "Iron Rod"

        Screw ->
            "Screw"

        ReinforcedIronPlate ->
            "Reinforced Iron Plate"

        Concrete ->
            "Concrete"

        Rotor ->
            "Rotor"

        Stator ->
            "Stator"

        Motor ->
            "Motor"

        ModularFrame ->
            "Modular Frame"

        SmartPlating ->
            "Smart Plating"

        IronOre ->
            "Iron Ore"

        CopperOre ->
            "Copper Ore"

        Water ->
            "Water"

        Coal ->
            "Coal"

        LimeStone ->
            "Limestone"

        Steel ->
            "Steel"

        SteelBeam ->
            "Steel Beam"

        SteelPipe ->
            "Steel Pipe"

        IndustrialSteelBeam ->
            "Industrial Steel Beam"

        VersatileFramework ->
            "Versatile Framework"

        AutomaticWire ->
            "Automatic Wire"

        CateriumOre ->
            "Caterium Ore"

        CateriumIngot ->
            "Caterium Ingot"

        Quickwire ->
            "Quickwire"

        Fuel ->
            "Fuel"

        Coke ->
            "Coke"

        Plastic ->
            "Plastic"

        Rubber ->
            "Rubber"

        HeavyOilResidue ->
            "Heavy Oil Residue"

        PolymerResidue ->
            "Polymer Residue"

        Oil ->
            "Oil"


parse : String -> Maybe IntermediateProduct
parse string =
    let
        predicate : IntermediateProduct -> Bool
        predicate product =
            text product == string
    in
    listFind predicate all
