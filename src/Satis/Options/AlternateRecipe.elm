module Satis.Options.AlternateRecipe exposing (AltRecipe(..), all, asOptions, text)

import Option exposing (Options)



{- type -}


type AltRecipe
    = BoltedModularFrame
    | QuickwireStator
    | FusedWire
    | CopperRotor
    | SteamedCopperSheet



{- utils -}


all : List AltRecipe
all =
    [ BoltedModularFrame, QuickwireStator, FusedWire, CopperRotor, SteamedCopperSheet ]


text : AltRecipe -> String
text option =
    case option of
        BoltedModularFrame ->
            "Bolted Modular Frame"

        FusedWire ->
            "Fused Wire"

        QuickwireStator ->
            "Quickwire Stator"

        CopperRotor ->
            "Copper Rotor"

        SteamedCopperSheet ->
            "Steamed Copper Sheet"


asOptions : Options AltRecipe
asOptions =
    Option.make all text
