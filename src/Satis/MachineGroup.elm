module Satis.MachineGroup exposing (ConsumeResult(..), MachineGroup, add, availableThrougtput, byproduct, consumeItems, consumption, count, hasAvailableThrougput, input, itemIOToMachineGroup, machine, machineGroupView, make, order, output, production)

import Element exposing (Element, el, row, text)
import Element.Font as Font
import Option exposing (Options)
import Satis.Energy as Energy exposing (MW)
import Satis.IO as IO exposing (ItemIO)
import Satis.Options.AlternateRecipe exposing (AltRecipe(..))
import Satis.Product as Product exposing (IntermediateProduct(..))
import Satis.Recipe as Recipe exposing (Recipe)
import Ui


type MachineGroup
    = MachineGroup
        { what : Recipe
        , count_ : Int
        , availableThrougtput_ : ItemIO
        }


type ConsumeResult
    = Sufficient MachineGroup
    | Insufficient MachineGroup ItemIO
    | NoThrougtput ItemIO


input : MachineGroup -> List ItemIO
input (MachineGroup { what, count_ }) =
    what
        |> Recipe.input
        |> List.map (IO.mul count_)


output : MachineGroup -> ItemIO
output (MachineGroup { what, count_ }) =
    what
        |> Recipe.output
        |> IO.mul count_


production : MachineGroup -> IntermediateProduct
production (MachineGroup { what }) =
    Recipe.output what
        |> IO.getProduct


machine : MachineGroup -> String
machine (MachineGroup { what }) =
    Recipe.assemblyMachineText what


count : MachineGroup -> Int
count (MachineGroup { count_ }) =
    count_


availableThrougtput : MachineGroup -> ItemIO
availableThrougtput (MachineGroup { availableThrougtput_ }) =
    availableThrougtput_


add : MachineGroup -> MachineGroup -> MachineGroup
add (MachineGroup a) (MachineGroup b) =
    MachineGroup
        { what = a.what
        , count_ = a.count_ + b.count_
        , availableThrougtput_ = IO.add a.availableThrougtput_ b.availableThrougtput_
        }


hasAvailableThrougput : MachineGroup -> Bool
hasAvailableThrougput (MachineGroup { availableThrougtput_ }) =
    IO.isEmpty availableThrougtput_
        |> not


consumeItems : ItemIO -> MachineGroup -> ConsumeResult
consumeItems io (MachineGroup group) =
    let
        quantityToConsume =
            IO.getQuantity io
    in
    if not <| hasAvailableThrougput (MachineGroup group) then
        NoThrougtput io

    else if IO.canHandle quantityToConsume group.availableThrougtput_ then
        let
            newThrougtput =
                IO.diminish quantityToConsume group.availableThrougtput_
        in
        Sufficient
            (MachineGroup { group | availableThrougtput_ = newThrougtput })

    else
        let
            newQuantity =
                quantityToConsume - IO.getQuantity group.availableThrougtput_
        in
        Insufficient
            (MachineGroup { group | availableThrougtput_ = IO.setQuantity 0 io })
            (IO.setQuantity newQuantity io)


order : MachineGroup -> Int
order (MachineGroup { what }) =
    Recipe.order what


itemIOToMachineGroup : Options AltRecipe -> ItemIO -> MachineGroup
itemIOToMachineGroup activatedAltRecipes io =
    let
        recipeToMachineGroup : Recipe -> MachineGroup
        recipeToMachineGroup recipe =
            let
                perMachineQuantity =
                    IO.getQuantity (Recipe.output recipe)

                machineNeeded =
                    IO.getQuantity io
                        / perMachineQuantity
                        |> ceiling

                rest =
                    (toFloat machineNeeded
                        * perMachineQuantity
                    )
                        - IO.getQuantity io

                thougtput =
                    IO.perMinute rest (IO.getProduct io)
            in
            MachineGroup
                { what = recipe
                , count_ = machineNeeded
                , availableThrougtput_ = thougtput
                }
    in
    Recipe.getRecipeOf activatedAltRecipes (IO.getProduct io)
        |> recipeToMachineGroup


byproduct : MachineGroup -> Maybe ItemIO
byproduct (MachineGroup { what, count_ }) =
    Recipe.byproduct what
        |> Maybe.map (IO.mul count_)


consumption : MachineGroup -> MW
consumption (MachineGroup { what, count_ }) =
    what
        |> Recipe.consumption
        |> Energy.mul count_


make : Recipe -> Int -> ItemIO -> MachineGroup
make recipe count_ availableThrougtput_ =
    MachineGroup
        { what = recipe
        , count_ = count_
        , availableThrougtput_ = availableThrougtput_
        }



{- view -}


machineGroupView : MachineGroup -> Element msg
machineGroupView (MachineGroup { what, count_, availableThrougtput_ }) =
    let
        remaining =
            if IO.isEmpty availableThrougtput_ then
                Element.none

            else
                text <| "with a surplus of " ++ IO.text availableThrougtput_

        byproduct_ =
            case Recipe.byproduct what of
                Nothing ->
                    Element.none

                Just io ->
                    row [] [ text "and ", IO.view io ]
    in
    row [ Element.spacing (Ui.space Ui.SmallSpace) ]
        [ Element.text (String.fromInt count_)
        , el [ Font.light, Ui.tooltip Element.above (Recipe.viewTooltip what), Element.pointer ] (text (Recipe.assemblyMachineText what))
        , text "producing"
        , el [ Font.light ] (text (Product.text <| IO.getProduct <| Recipe.output <| what))
        , remaining
        , byproduct_
        ]
