module ProductDependencyGraph exposing (ProductGraphModel, ProductGraphMsg, initProductGraph, updateProductGraph, viewProductGraph)

import Array exposing (Array)
import Dict exposing (Dict)
import Element exposing (Element, column, el, row, text)
import Element.Font as Font
import Element.Input as Input
import Graph exposing (Graph)
import Html exposing (Html, button, div, input, option, select)
import Html.Attributes exposing (selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List
import Stylesheet exposing (edges, stylesheetColor, stylesheetFontsize, stylesheetSpacing)



-- type Transport
--     = BeltMk1
--     | BeltMk2
--     | Pipe
--
--
-- itemPerBelt : Belt -> Int
-- itemPerBelt belt =
--     case belt of
--         BeltMk1 ->
--             60
--
--         BeltMk2 ->
--             120
--
--
-- minimalBeltMkForQuantity : Int -> Belt
-- minimalBeltMkForQuantity quantity =
--     if quantity <= 60 then
--         BeltMk1
--
--     else
--         BeltMk2


type alias ActivatedAltRecipes =
    Dict String Bool


type alias ProductGraphModel =
    { needs : Array ItemIO
    , altRecipes : ActivatedAltRecipes
    }


type ProductGraphMsg
    = SelectProduct Int IntermediateProduct
    | UpdateQuantity Int Float
    | AddProductNeed
    | RemoveProductNeed Int
    | UpdateAltRecipe String Bool


type AltRecipe
    = BoltedModularFrame


type EnergyGenerator
    = BiomassBurner
    | CoalGenerator


powerPerGenerator : EnergyGenerator -> MW
powerPerGenerator generator =
    case generator of
        BiomassBurner ->
            MW 30

        CoalGenerator ->
            MW 75


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
            [ ItemIO 15 Coal
            , ItemIO 45 Water
            ]


altRecipeText : AltRecipe -> String
altRecipeText recipe =
    case recipe of
        BoltedModularFrame ->
            "Bolted Modular Frame"


initProductGraph : ProductGraphModel
initProductGraph =
    { needs = Array.fromList [ perMinute 2 SmartPlating ]
    , altRecipes = allAltRecipes |> List.map (\key -> ( altRecipeText key, False )) |> Dict.fromList
    }


parseProduct : String -> Maybe IntermediateProduct
parseProduct string =
    case string of
        "Copper Ingot" ->
            Just CopperIngot

        "Copper Wire" ->
            Just CopperWire

        "Copper Sheet" ->
            Just CopperSheet

        "Iron Ingot" ->
            Just IronIngot

        "Iron Plate" ->
            Just IronPlate

        "Iron Rod" ->
            Just IronRod

        "Screw" ->
            Just Screw

        "Reinforced Iron Plate" ->
            Just ReinforcedIronPlate

        "Concrete" ->
            Just Concrete

        "Rotor" ->
            Just Rotor

        "Modular Frame" ->
            Just ModularFrame

        "Smart Plating" ->
            Just SmartPlating

        "Steel" ->
            Just Steel

        "Steel Beam" ->
            Just SteelBeam

        "Steel Pipe" ->
            Just SteelPipe

        "Industrial Steel Beam" ->
            Just IndustrialSteelBeam

        "Stator" ->
            Just Stator

        "Motor" ->
            Just Motor

        "Versatile Framework" ->
            Just VersatileFramework

        "Automatic Wire" ->
            Just AutomaticWire

        "Coal" ->
            Just Coal

        "Water" ->
            Just Water

        _ ->
            Nothing


onProductUpdate : Int -> String -> ProductGraphMsg
onProductUpdate index product =
    product
        |> parseProduct
        |> Maybe.withDefault IronPlate
        |> SelectProduct index


onQuantityUpdate : Int -> String -> ProductGraphMsg
onQuantityUpdate index quantity =
    quantity
        |> String.toFloat
        |> Maybe.withDefault 1
        |> UpdateQuantity index


updateProductGraph : ProductGraphMsg -> ProductGraphModel -> ProductGraphModel
updateProductGraph msg model =
    case msg of
        UpdateQuantity index quantity ->
            case Array.get index model.needs of
                Nothing ->
                    model

                Just io ->
                    let
                        newNeeds =
                            Array.set index (setQuantity quantity io) model.needs
                    in
                    { model | needs = newNeeds }

        SelectProduct index product ->
            case Array.get index model.needs of
                Nothing ->
                    model

                Just io ->
                    let
                        newNeeds =
                            Array.set index (setProduct product io) model.needs
                    in
                    { model | needs = newNeeds }

        AddProductNeed ->
            let
                newNeeds =
                    Array.push (perMinute 1 IronPlate) model.needs
            in
            { model | needs = newNeeds }

        RemoveProductNeed index ->
            let
                newNeeds =
                    removeIndexInArray index model.needs
            in
            { model | needs = newNeeds }

        UpdateAltRecipe recipe bool ->
            { model | altRecipes = Dict.insert recipe bool model.altRecipes }


removeIndexInArray : Int -> Array a -> Array a
removeIndexInArray index array =
    let
        before =
            Array.slice 0 index array

        after =
            Array.slice (index + 1) (Array.length array) array
    in
    Array.append before after


type MW
    = MW Int


makeMW : Int -> MW
makeMW int =
    MW int


mulMW : Int -> MW -> MW
mulMW coef (MW a) =
    MW (a * coef)


addMW : MW -> MW -> MW
addMW (MW a) (MW b) =
    MW (a + b)


textMW : MW -> String
textMW (MW a) =
    String.fromInt a
        ++ "MW"


type AssemblyMachine
    = MinerMk1
    | Smelter
    | Constructor
    | Assembler
    | Pump
    | Foundry


machineConsumption : AssemblyMachine -> MW
machineConsumption machine =
    case machine of
        Assembler ->
            MW 15

        Constructor ->
            MW 4

        Smelter ->
            MW 12

        Pump ->
            MW 20

        MinerMk1 ->
            MW 5

        Foundry ->
            MW 16


type ItemIO
    = ItemIO Float IntermediateProduct


perMinute : Float -> IntermediateProduct -> ItemIO
perMinute quantity item =
    ItemIO quantity item



-- TODO encure we're not adding 2 different kind of item together


addItemIO : ItemIO -> ItemIO -> ItemIO
addItemIO (ItemIO a item) (ItemIO b _) =
    ItemIO (a + b) item


mulItemIO : Int -> ItemIO -> ItemIO
mulItemIO coef (ItemIO quantity item) =
    let
        floatCoef =
            toFloat coef
    in
    ItemIO (floatCoef * quantity) item


emptyItemIO : ItemIO -> Bool
emptyItemIO (ItemIO quantity _) =
    quantity <= 0


canHandle : Float -> ItemIO -> Bool
canHandle target (ItemIO quantity _) =
    quantity >= target


diminishIO : Float -> ItemIO -> ItemIO
diminishIO toRemove (ItemIO quantity item) =
    let
        newQuantity =
            max 0 (quantity - toRemove)
    in
    ItemIO newQuantity item


getQuantity : ItemIO -> Float
getQuantity (ItemIO quantity _) =
    quantity


setQuantity : Float -> ItemIO -> ItemIO
setQuantity quantity (ItemIO _ item) =
    ItemIO quantity item


getProduct : ItemIO -> IntermediateProduct
getProduct (ItemIO _ product) =
    product


setProduct : IntermediateProduct -> ItemIO -> ItemIO
setProduct product (ItemIO quantity _) =
    ItemIO quantity product


itemIOToMachineGroup : ActivatedAltRecipes -> ItemIO -> MachineGroup
itemIOToMachineGroup activatedAltRecipes (ItemIO targetQuantity targetItem) =
    let
        recipeToMachineGroup : Recipe -> MachineGroup
        recipeToMachineGroup (Recipe { machine, output, input }) =
            let
                perMachineQuantity =
                    getQuantity output

                machineNeeded =
                    targetQuantity
                        / perMachineQuantity
                        |> ceiling

                rest =
                    toFloat machineNeeded
                        * perMachineQuantity
                        - targetQuantity

                thougtput =
                    ItemIO rest targetItem
            in
            MachineGroup
                { kind = machine
                , count = machineNeeded
                , availableThrougtput = thougtput
                , producing = targetItem
                , inputPerMachine = input
                }
    in
    getRecipeOf activatedAltRecipes targetItem
        |> recipeToMachineGroup


type Recipe
    = Recipe
        { machine : AssemblyMachine
        , input : List ItemIO
        , output : ItemIO
        }


type
    IntermediateProduct
    -- Raw
    = IronOre
    | CopperOre
    | Coal
    | Water
    | LimeStone
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


allProducts : List IntermediateProduct
allProducts =
    [ CopperWire, CopperCable, CopperSheet, IronPlate, IronRod, Screw, ReinforcedIronPlate, Rotor, ModularFrame, SmartPlating, Steel, SteelBeam, SteelPipe, IndustrialSteelBeam, Stator, Motor, VersatileFramework, AutomaticWire ]


allAltRecipes : List AltRecipe
allAltRecipes =
    [ BoltedModularFrame ]


getMachingeGroupOrderIndex : MachineGroup -> Int
getMachingeGroupOrderIndex (MachineGroup { producing }) =
    case producing of
        Coal ->
            100

        Water ->
            200

        LimeStone ->
            1000

        Concrete ->
            1100

        CopperOre ->
            2000

        CopperIngot ->
            2050

        CopperWire ->
            2200

        CopperCable ->
            2300

        CopperSheet ->
            2100

        IronOre ->
            3000

        IronIngot ->
            3050

        IronPlate ->
            3100

        IronRod ->
            3200

        Screw ->
            3300

        ReinforcedIronPlate ->
            3500

        Rotor ->
            5200

        ModularFrame ->
            5300

        SmartPlating ->
            7100

        Steel ->
            4100

        SteelBeam ->
            4200

        SteelPipe ->
            4300

        IndustrialSteelBeam ->
            4500

        Stator ->
            5500

        Motor ->
            5600

        VersatileFramework ->
            7200

        AutomaticWire ->
            7300


isRecipeActivated : AltRecipe -> ActivatedAltRecipes -> Bool
isRecipeActivated recipe recipes =
    let
        key =
            altRecipeText recipe
    in
    recipes
        |> Dict.get key
        |> Maybe.withDefault False


getRecipeOf : ActivatedAltRecipes -> IntermediateProduct -> Recipe
getRecipeOf recipes product =
    case product of
        IronOre ->
            Recipe
                { machine = MinerMk1
                , input = []
                , output = perMinute 60 IronOre
                }

        CopperOre ->
            Recipe
                { machine = MinerMk1
                , input = []
                , output = perMinute 60 CopperOre
                }

        Coal ->
            Recipe
                { machine = MinerMk1
                , input = []
                , output = perMinute 60 Coal
                }

        Water ->
            Recipe
                { machine = Pump
                , input = []
                , output = perMinute 120 Water
                }

        LimeStone ->
            Recipe
                { machine = MinerMk1
                , input = []
                , output = perMinute 60 LimeStone
                }

        Concrete ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 45 LimeStone ]
                , output = perMinute 15 Concrete
                }

        CopperWire ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 15 CopperIngot ]
                , output = perMinute 30 CopperWire
                }

        CopperSheet ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 20 CopperIngot ]
                , output = perMinute 10 CopperSheet
                }

        CopperCable ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 60 CopperWire ]
                , output = perMinute 30 CopperCable
                }

        IronPlate ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 30 IronIngot ]
                , output = perMinute 20 IronPlate
                }

        IronRod ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 15 IronIngot ]
                , output = perMinute 15 IronRod
                }

        Screw ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 10 IronRod ]
                , output = perMinute 40 Screw
                }

        ReinforcedIronPlate ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 30 IronPlate, perMinute 60 Screw ]
                , output = perMinute 5 ReinforcedIronPlate
                }

        Rotor ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 20 IronRod, perMinute 100 Screw ]
                , output = perMinute 4 Rotor
                }

        SmartPlating ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 2 ReinforcedIronPlate, perMinute 2 Rotor ]
                , output = perMinute 2 SmartPlating
                }

        ModularFrame ->
            if isRecipeActivated BoltedModularFrame recipes then
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 7.5 ReinforcedIronPlate, perMinute 140 Screw ]
                    , output = perMinute 5 ModularFrame
                    }

            else
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 3 ReinforcedIronPlate, perMinute 12 IronRod ]
                    , output = perMinute 2 ModularFrame
                    }

        Steel ->
            Recipe
                { machine = Foundry
                , input = [ perMinute 45 IronOre, perMinute 45 Coal ]
                , output = perMinute 45 Steel
                }

        SteelPipe ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 30 Steel ]
                , output = perMinute 20 SteelPipe
                }

        SteelBeam ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 60 Steel ]
                , output = perMinute 15 SteelBeam
                }

        IndustrialSteelBeam ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 24 Steel, perMinute 30 Concrete ]
                , output = perMinute 6 IndustrialSteelBeam
                }

        VersatileFramework ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 2.5 ModularFrame, perMinute 30 SteelBeam ]
                , output = perMinute 5 VersatileFramework
                }

        AutomaticWire ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 2.5 Stator, perMinute 50 CopperWire ]
                , output = perMinute 2.5 AutomaticWire
                }

        Stator ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 15 SteelPipe, perMinute 8 CopperWire ]
                , output = perMinute 5 Stator
                }

        Motor ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 10 Rotor, perMinute 10 Stator ]
                , output = perMinute 5 Motor
                }

        IronIngot ->
            Recipe
                { machine = Smelter
                , input = [ perMinute 30 IronOre ]
                , output = perMinute 30 IronIngot
                }

        CopperIngot ->
            Recipe
                { machine = Smelter
                , input = [ perMinute 30 CopperOre ]
                , output = perMinute 30 CopperIngot
                }



-- Merge to principle of machine with the recipe to limit io ?


type MachineGroup
    = MachineGroup
        { kind : AssemblyMachine
        , count : Int
        , availableThrougtput : ItemIO
        , producing : IntermediateProduct
        , inputPerMachine : List ItemIO
        }


addMachineGroup : MachineGroup -> MachineGroup -> MachineGroup
addMachineGroup (MachineGroup a) (MachineGroup b) =
    MachineGroup
        { kind = a.kind
        , inputPerMachine = a.inputPerMachine
        , count = a.count + b.count
        , availableThrougtput = addItemIO a.availableThrougtput b.availableThrougtput
        , producing = a.producing
        }


machineGroupInput : MachineGroup -> List ItemIO
machineGroupInput (MachineGroup { inputPerMachine, count }) =
    inputPerMachine
        |> List.map (mulItemIO count)


type alias ProductionLane =
    Dict String MachineGroup


hasAvailableThrougput : MachineGroup -> Bool
hasAvailableThrougput (MachineGroup { availableThrougtput }) =
    emptyItemIO availableThrougtput
        |> not


type ConsumeResult
    = Sufficient MachineGroup
    | Insufficient MachineGroup ItemIO
    | NoThrougtput ItemIO


consumeMachineGroupItems : ItemIO -> MachineGroup -> ConsumeResult
consumeMachineGroupItems io (MachineGroup group) =
    let
        quantityToConsume =
            getQuantity io
    in
    if not <| hasAvailableThrougput (MachineGroup group) then
        NoThrougtput io

    else if canHandle quantityToConsume group.availableThrougtput then
        let
            newThrougtput =
                diminishIO quantityToConsume group.availableThrougtput
        in
        Sufficient
            (MachineGroup { group | availableThrougtput = newThrougtput })

    else
        let
            newQuantity =
                quantityToConsume - getQuantity group.availableThrougtput
        in
        Insufficient
            (MachineGroup { group | availableThrougtput = setQuantity 0 io })
            (setQuantity newQuantity io)


addProductionNeeds : ActivatedAltRecipes -> ItemIO -> ProductionLane -> ( ProductionLane, List ItemIO )
addProductionNeeds activatedAltRecipes io lane =
    let
        itemName =
            intermediateProductText (getProduct io)
    in
    case Dict.get itemName lane of
        Nothing ->
            let
                group =
                    itemIOToMachineGroup activatedAltRecipes io
            in
            ( Dict.insert itemName group lane, machineGroupInput group )

        Just existingGroup ->
            case consumeMachineGroupItems io existingGroup of
                Sufficient mergedGroup ->
                    ( Dict.insert itemName mergedGroup lane, [] )

                Insufficient fullGroup missingItemIO ->
                    ( Dict.insert itemName fullGroup lane, [ missingItemIO ] )

                NoThrougtput missingItemIO ->
                    let
                        newGroup =
                            itemIOToMachineGroup activatedAltRecipes missingItemIO

                        groupToAdd =
                            addMachineGroup existingGroup newGroup
                    in
                    ( Dict.insert itemName groupToAdd lane, machineGroupInput newGroup )


getProductLaneFor : ActivatedAltRecipes -> List ItemIO -> ProductionLane
getProductLaneFor activatedAltRecipes io =
    getProductLaneForHelper activatedAltRecipes io Dict.empty


getProductLaneForHelper : ActivatedAltRecipes -> List ItemIO -> ProductionLane -> ProductionLane
getProductLaneForHelper activatedAltRecipes remaining lane =
    case remaining of
        [] ->
            lane

        io :: rest ->
            let
                ( newLane, missingIO ) =
                    addProductionNeeds activatedAltRecipes io lane
            in
            getProductLaneForHelper activatedAltRecipes (List.append rest missingIO) newLane



{- View -}


intermediateProductText : IntermediateProduct -> String
intermediateProductText product =
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


assemblyMachineText : AssemblyMachine -> String
assemblyMachineText machine =
    case machine of
        Constructor ->
            "Constructor"

        Assembler ->
            "Assembler"

        MinerMk1 ->
            "Miner Mk1"

        Pump ->
            "Pump"

        Smelter ->
            "Smelter"

        Foundry ->
            "Foundry"


itemIOText : ItemIO -> String
itemIOText (ItemIO num product) =
    String.fromFloat num
        ++ " "
        ++ intermediateProductText product


machineGroupView : MachineGroup -> Element ProductGraphMsg
machineGroupView (MachineGroup { kind, count, availableThrougtput, producing }) =
    let
        remaining =
            if emptyItemIO availableThrougtput then
                ""

            else
                " with a surplus of " ++ itemIOText availableThrougtput
    in
    String.fromInt count
        ++ " "
        ++ assemblyMachineText kind
        ++ " producing "
        ++ intermediateProductText producing
        ++ remaining
        |> text


surround : a -> a -> List a -> List a
surround start end list =
    List.concat [ [ start ], list, [ end ] ]


productionLaneView : ProductionLane -> Element ProductGraphMsg
productionLaneView lane =
    let
        wrapGroupViewInLi group =
            row [] [ machineGroupView group ]

        addConsumption _ (MachineGroup { kind, count }) mw =
            kind
                |> machineConsumption
                |> mulMW count
                |> addMW mw

        totalConsumption =
            lane
                |> Dict.foldl addConsumption (makeMW 0)

        neededGeneratorView ( generator, count ) =
            row
                [ Element.spacing (stylesheetSpacing Stylesheet.SmallSpace) ]
                [ text (String.fromInt count), viewGenerator generator ]
    in
    column []
        [ row
            [ Element.spacing (stylesheetSpacing Stylesheet.SmallSpace) ]
            [ text
                ("Total consumption: "
                    ++ textMW totalConsumption
                )
            , getNeededGenerator totalConsumption
                |> List.map neededGeneratorView
                |> List.intersperse (text " / ")
                |> surround (text "(") (text ")")
                |> row []
            ]
        , column
            []
            (lane
                |> Dict.values
                |> List.sortBy getMachingeGroupOrderIndex
                |> List.map wrapGroupViewInLi
            )
        ]


selectProductView : Int -> IntermediateProduct -> Element ProductGraphMsg
selectProductView index item =
    let
        productOptionView selectedProduct product =
            option
                [ value <| intermediateProductText product
                , selected <| selectedProduct == product
                ]
                [ Html.text <| intermediateProductText product ]

        inputSelect =
            Element.html <|
                select
                    [ onInput (onProductUpdate index), style "height" "40px" ]
                    (List.map (productOptionView item) allProducts)
    in
    el [] inputSelect


updateProductQuantityView : Int -> Float -> Element ProductGraphMsg
updateProductQuantityView index quantity =
    Input.text
        [ Element.htmlAttribute <| type_ "number"
        , Element.htmlAttribute <| Html.Attributes.min "1"
        , Element.width (Element.px 72)
        ]
        { onChange = onQuantityUpdate index
        , text = String.fromFloat quantity
        , placeholder = Nothing
        , label = Input.labelHidden "Product Quantity"
        }


itemIOInputView : Int -> ItemIO -> Element ProductGraphMsg
itemIOInputView index (ItemIO quantity product) =
    let
        removeButton =
            if index /= 0 then
                Input.button
                    [ Font.color (stylesheetColor Stylesheet.DangerColor) ]
                    { onPress = Just (RemoveProductNeed index), label = text "Remove" }

            else
                Element.none
    in
    row [ Element.spacing (stylesheetSpacing Stylesheet.SmallSpace) ]
        [ updateProductQuantityView index quantity
        , selectProductView index product
        , removeButton
        ]


editionLineView : ProductGraphModel -> Element ProductGraphMsg
editionLineView { needs } =
    let
        needsInputs =
            needs
                |> Array.toList
                |> List.indexedMap itemIOInputView

        addButton =
            Input.button
                [ Font.color (stylesheetColor Stylesheet.InfoColor) ]
                { onPress = Just AddProductNeed, label = text "+ Add" }
    in
    column [ Element.spacing (stylesheetSpacing Stylesheet.SmallSpace) ]
        (List.concat
            [ [ text "I want to produce every minute" ]
            , needsInputs
            , [ addButton ]
            ]
        )


toggleAltRecipesView : ActivatedAltRecipes -> Element ProductGraphMsg
toggleAltRecipesView recipes =
    let
        altRecipeView ( key, value ) =
            Input.checkbox []
                { onChange = UpdateAltRecipe key
                , icon = Input.defaultCheckbox
                , checked = value
                , label = Input.labelRight [] (text key)
                }
    in
    column []
        [ text "Alternative recipes "
        , column [] (recipes |> Dict.toList |> List.map altRecipeView)
        ]


viewProductGraph : ProductGraphModel -> Element ProductGraphMsg
viewProductGraph model =
    let
        lane =
            model.needs
                |> Array.toList
                |> getProductLaneFor model.altRecipes
    in
    column [ Element.spacing (stylesheetSpacing Stylesheet.RegularSpace) ]
        [ toggleAltRecipesView model.altRecipes
        , editionLineView model
        , productionLaneView lane
        ]


viewGenerator : EnergyGenerator -> Element ProductGraphMsg
viewGenerator generator =
    case generator of
        BiomassBurner ->
            el
                [ Font.color (stylesheetColor Stylesheet.GreenColor)
                , Font.bold
                ]
                (text "Biomass Burner")

        CoalGenerator ->
            el
                [ Font.color (stylesheetColor Stylesheet.BlackColor)
                , Font.bold
                ]
                (text "Coal Generator")
