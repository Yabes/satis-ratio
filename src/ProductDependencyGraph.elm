module ProductDependencyGraph exposing (ProductGraphModel, ProductGraphMsg, encodeGraph, initProductGraph, updateProductGraph, viewProductGraph)

import Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import Element exposing (Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graph exposing (Graph)
import Html exposing (button, option, select)
import Html.Attributes exposing (selected, style, type_, value)
import Html.Events exposing (onInput)
import Json.Encode as E
import List
import Ui exposing (color, space)



{- Types -}


type ProductGraphMsg
    = SelectProduct Int IntermediateProduct
    | UpdateQuantity Int Float
    | AddProductNeed
    | RemoveProductNeed Int
    | UpdateAltRecipe String Bool
    | UpdateProductionOption String Bool
    | UpdateByProducts ByproductUsage


type EnergyGenerator
    = BiomassBurner
    | CoalGenerator
    | FuelGenerator


type Recipe
    = -- Extractor
      Miner ItemIO
    | WaterExtractor ItemIO
    | OilExtractor ItemIO
      -- Furnaces
    | Smelter { input : ItemIO, output : ItemIO }
    | Foundry { input : ( ItemIO, ItemIO ), output : ItemIO }
      -- Transformation
    | Constructor { input : ItemIO, output : ItemIO }
    | Assembler { input : ( ItemIO, ItemIO ), output : ItemIO }
    | Refinery { input : ( ItemIO, Maybe ItemIO ), output : ItemIO, byproduct : Maybe ItemIO }
    | Residue


type MachineGroup
    = MachineGroup
        { what : Recipe
        , count : Int
        , availableThrougtput : ItemIO
        }


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


type AltRecipe
    = BoltedModularFrame
    | QuickwireStator
    | FusedWire
    | CopperRotor
    | SteamedCopperSheet


type ProductionOption
    = IncludeResourceExtractor


type ByproductUsage
    = None
    | Balanced { heavyOilResidueRatio : Float, polymerResidueRatio : Float }


type alias ProductionLane =
    Dict String MachineGroup


type alias ActivatedAltRecipes =
    Dict String Bool


type alias ProductionOptions =
    Dict String Bool


type alias ProductionLaneGraph =
    Graph MachineGroup ItemIO


type alias ProductGraphModel =
    { needs : Array ItemIO
    , altRecipes : ActivatedAltRecipes
    , productionOptions : ProductionOptions
    , byproductUsage : ByproductUsage
    }


allProducts : List IntermediateProduct
allProducts =
    [ CopperWire, CopperCable, CopperSheet, IronPlate, IronRod, Screw, ReinforcedIronPlate, Rotor, ModularFrame, SmartPlating, Steel, SteelBeam, SteelPipe, IndustrialSteelBeam, Stator, Motor, VersatileFramework, AutomaticWire, Plastic, Rubber, Fuel ]


allAltRecipes : List AltRecipe
allAltRecipes =
    [ BoltedModularFrame, QuickwireStator, FusedWire, CopperRotor, SteamedCopperSheet ]


allProductionOptions : List ProductionOption
allProductionOptions =
    [ IncludeResourceExtractor ]



{- Utils -}


recipeInput : Recipe -> List ItemIO
recipeInput recipe =
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

        Smelter { input } ->
            [ input ]

        Foundry { input } ->
            tupleToList input

        Constructor { input } ->
            [ input ]

        Assembler { input } ->
            tupleToList input

        Refinery { input } ->
            optionalTupleToList input

        Residue ->
            Debug.todo ""


machineGroupInput : MachineGroup -> List ItemIO
machineGroupInput (MachineGroup { what, count }) =
    what
        |> recipeInput
        |> List.map (mulItemIO count)


machineGroupOutput : MachineGroup -> ItemIO
machineGroupOutput (MachineGroup { what, count }) =
    what
        |> recipeOutput
        |> mulItemIO count


recipeOutput : Recipe -> ItemIO
recipeOutput recipe =
    case recipe of
        Miner output ->
            output

        WaterExtractor output ->
            output

        OilExtractor output ->
            output

        Smelter { output } ->
            output

        Foundry { output } ->
            output

        Constructor { output } ->
            output

        Assembler { output } ->
            output

        Refinery { output } ->
            output

        Residue ->
            Debug.todo "Find a way to avoid Residue in Recipe"


recipeByproduct : Recipe -> Maybe ItemIO
recipeByproduct recipe =
    case recipe of
        Refinery { byproduct } ->
            byproduct

        _ ->
            Nothing


recipeOutputWithByroduct : Recipe -> List ItemIO
recipeOutputWithByroduct recipe =
    let
        out =
            recipeOutput recipe

        optionalByproduct =
            recipeByproduct recipe
    in
    case optionalByproduct of
        Nothing ->
            [ out ]

        Just byproduct ->
            [ out, byproduct ]


machineGroupProduction : MachineGroup -> IntermediateProduct
machineGroupProduction (MachineGroup { what }) =
    let
        (ItemIO _ product) =
            recipeOutput what
    in
    product


machineGroupMachine : MachineGroup -> String
machineGroupMachine (MachineGroup { what }) =
    assemblyMachineText what


machineGroupCount : MachineGroup -> Int
machineGroupCount (MachineGroup { count }) =
    count


machineGroupExtraneous : MachineGroup -> ItemIO
machineGroupExtraneous (MachineGroup { availableThrougtput }) =
    availableThrougtput


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
            [ ItemIO 15 Coal
            , ItemIO 45 Water
            ]

        FuelGenerator ->
            [ ItemIO 15 Oil ]


altRecipeText : AltRecipe -> String
altRecipeText option =
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


productionOptionText : ProductionOption -> String
productionOptionText option =
    case option of
        IncludeResourceExtractor ->
            "Include Resource Extractor"


initProductGraph : ProductGraphModel
initProductGraph =
    { needs = Array.fromList [ perMinute 2 SmartPlating ]
    , altRecipes = allAltRecipes |> List.map (\key -> ( altRecipeText key, False )) |> Dict.fromList
    , productionOptions = allProductionOptions |> List.map (\key -> ( productionOptionText key, False )) |> Dict.fromList
    , byproductUsage = None
    }


listFind : (a -> Bool) -> List a -> Maybe a
listFind fn list =
    case list of
        [] ->
            Nothing

        head :: rest ->
            if fn head then
                Just head

            else
                listFind fn rest


listIndex : (a -> Bool) -> List a -> Maybe Int
listIndex fn list =
    listIndexHelper 0 fn list


listIndexHelper : Int -> (a -> Bool) -> List a -> Maybe Int
listIndexHelper index fn list =
    case list of
        [] ->
            Nothing

        head :: rest ->
            if fn head then
                Just index

            else
                listIndexHelper (index + 1) fn rest


parseProduct : String -> Maybe IntermediateProduct
parseProduct string =
    let
        predicate : IntermediateProduct -> Bool
        predicate product =
            intermediateProductText product == string
    in
    listFind predicate allProducts


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

        UpdateProductionOption recipe bool ->
            { model | productionOptions = Dict.insert recipe bool model.productionOptions }

        UpdateByProducts newUsage ->
            { model | byproductUsage = newUsage }


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


mwView : MW -> Element msg
mwView (MW a) =
    let
        innerText =
            String.fromInt a ++ "MW"
    in
    el [ Font.bold, Font.color <| color Ui.YellowColor ] (text innerText)


recipeConsumption : Recipe -> MW
recipeConsumption recipe =
    case recipe of
        Assembler _ ->
            MW 15

        Constructor _ ->
            MW 4

        Smelter _ ->
            MW 12

        WaterExtractor _ ->
            MW 20

        Miner _ ->
            MW 5

        Foundry _ ->
            MW 16

        Refinery _ ->
            MW 30

        OilExtractor _ ->
            MW 40

        Residue ->
            Debug.todo ""


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
        recipeToMachineGroup recipe =
            let
                perMachineQuantity =
                    getQuantity (recipeOutput recipe)

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
                { what = recipe
                , count = machineNeeded
                , availableThrougtput = thougtput
                }
    in
    getRecipeOf activatedAltRecipes targetItem
        |> recipeToMachineGroup


getMachingeGroupOrderIndex : MachineGroup -> Int
getMachingeGroupOrderIndex (MachineGroup { what }) =
    case what of
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

        Residue ->
            Debug.todo ""


isOptionActivated : ProductionOption -> ProductionOptions -> Bool
isOptionActivated option options =
    let
        key =
            productionOptionText option
    in
    options
        |> Dict.get key
        |> Maybe.withDefault False


isAltRecipeActivated : AltRecipe -> ActivatedAltRecipes -> Bool
isAltRecipeActivated recipe recipes =
    let
        key =
            altRecipeText recipe
    in
    recipes
        |> Dict.get key
        |> Maybe.withDefault False


productColor : IntermediateProduct -> Color
productColor product =
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


getRecipeOf : ActivatedAltRecipes -> IntermediateProduct -> Recipe
getRecipeOf options product =
    case product of
        IronOre ->
            Miner <| perMinute 60 IronOre

        CopperOre ->
            Miner <| perMinute 60 CopperOre

        CateriumOre ->
            Miner <| perMinute 60 CateriumOre

        Coal ->
            Miner <| perMinute 60 Coal

        Water ->
            WaterExtractor <| perMinute 120 Water

        LimeStone ->
            Miner <| perMinute 60 LimeStone

        Concrete ->
            Constructor
                { input = perMinute 45 LimeStone
                , output = perMinute 15 Concrete
                }

        CateriumIngot ->
            Smelter
                { input = perMinute 45 CateriumOre
                , output = perMinute 15 CateriumIngot
                }

        Quickwire ->
            Constructor
                { input = perMinute 12 CateriumIngot
                , output = perMinute 60 Quickwire
                }

        CopperWire ->
            if isAltRecipeActivated FusedWire options then
                Assembler
                    { input = ( perMinute 70 CopperIngot, perMinute 7.5 CateriumIngot )
                    , output = perMinute 225 CopperWire
                    }

            else
                Constructor
                    { input = perMinute 15 CopperIngot
                    , output = perMinute 30 CopperWire
                    }

        CopperSheet ->
            if isAltRecipeActivated SteamedCopperSheet options then
                Refinery
                    { input = ( perMinute 27.5 Water, Just <| perMinute 27.5 CopperIngot )
                    , output = perMinute 27.5 CopperSheet
                    , byproduct = Nothing
                    }

            else
                Constructor
                    { input = perMinute 20 CopperIngot
                    , output = perMinute 10 CopperSheet
                    }

        CopperCable ->
            Constructor
                { input = perMinute 60 CopperWire
                , output = perMinute 30 CopperCable
                }

        IronPlate ->
            Constructor
                { input = perMinute 30 IronIngot
                , output = perMinute 20 IronPlate
                }

        IronRod ->
            Constructor
                { input = perMinute 15 IronIngot
                , output = perMinute 15 IronRod
                }

        Screw ->
            Constructor
                { input = perMinute 10 IronRod
                , output = perMinute 40 Screw
                }

        ReinforcedIronPlate ->
            Assembler
                { input = ( perMinute 30 IronPlate, perMinute 60 Screw )
                , output = perMinute 5 ReinforcedIronPlate
                }

        Rotor ->
            if isAltRecipeActivated CopperRotor options then
                Assembler
                    { input = ( perMinute 56.25 CopperSheet, perMinute 487.5 Screw )
                    , output = perMinute 28.1 Rotor
                    }

            else
                Assembler
                    { input = ( perMinute 20 IronRod, perMinute 100 Screw )
                    , output = perMinute 4 Rotor
                    }

        SmartPlating ->
            Assembler
                { input = ( perMinute 2 ReinforcedIronPlate, perMinute 2 Rotor )
                , output = perMinute 2 SmartPlating
                }

        ModularFrame ->
            if isAltRecipeActivated BoltedModularFrame options then
                Assembler
                    { input = ( perMinute 7.5 ReinforcedIronPlate, perMinute 140 Screw )
                    , output = perMinute 5 ModularFrame
                    }

            else
                Assembler
                    { input = ( perMinute 3 ReinforcedIronPlate, perMinute 12 IronRod )
                    , output = perMinute 2 ModularFrame
                    }

        Steel ->
            Foundry
                { input = ( perMinute 45 IronOre, perMinute 45 Coal )
                , output = perMinute 45 Steel
                }

        SteelPipe ->
            Constructor
                { input = perMinute 30 Steel
                , output = perMinute 20 SteelPipe
                }

        SteelBeam ->
            Constructor
                { input = perMinute 60 Steel
                , output = perMinute 15 SteelBeam
                }

        IndustrialSteelBeam ->
            Assembler
                { input = ( perMinute 24 Steel, perMinute 30 Concrete )
                , output = perMinute 6 IndustrialSteelBeam
                }

        VersatileFramework ->
            Assembler
                { input = ( perMinute 2.5 ModularFrame, perMinute 30 SteelBeam )
                , output = perMinute 5 VersatileFramework
                }

        AutomaticWire ->
            Assembler
                { input = ( perMinute 2.5 Stator, perMinute 50 CopperWire )
                , output = perMinute 2.5 AutomaticWire
                }

        Stator ->
            if isAltRecipeActivated QuickwireStator options then
                Assembler
                    { input = ( perMinute 40 SteelPipe, perMinute 150 Quickwire )
                    , output = perMinute 20 Stator
                    }

            else
                Assembler
                    { input = ( perMinute 15 SteelPipe, perMinute 8 CopperWire )
                    , output = perMinute 5 Stator
                    }

        Motor ->
            Assembler
                { input = ( perMinute 10 Rotor, perMinute 10 Stator )
                , output = perMinute 5 Motor
                }

        IronIngot ->
            Smelter
                { input = perMinute 30 IronOre
                , output = perMinute 30 IronIngot
                }

        CopperIngot ->
            Smelter
                { input = perMinute 30 CopperOre
                , output = perMinute 30 CopperIngot
                }

        Oil ->
            OilExtractor <| perMinute 120 Oil

        Fuel ->
            Refinery
                { input = ( perMinute 60 Oil, Nothing )
                , output = perMinute 40 Fuel
                , byproduct = Just (perMinute 30 PolymerResidue)
                }

        Plastic ->
            Refinery
                { input = ( perMinute 30 Oil, Nothing )
                , output = perMinute 20 Plastic
                , byproduct = Just (perMinute 10 HeavyOilResidue)
                }

        Rubber ->
            Refinery
                { input = ( perMinute 30 Oil, Nothing )
                , output = perMinute 20 Rubber
                , byproduct = Just (perMinute 20 HeavyOilResidue)
                }

        Coke ->
            Residue

        HeavyOilResidue ->
            Residue

        PolymerResidue ->
            Residue


addMachineGroup : MachineGroup -> MachineGroup -> MachineGroup
addMachineGroup (MachineGroup a) (MachineGroup b) =
    MachineGroup
        { what = a.what
        , count = a.count + b.count
        , availableThrougtput = addItemIO a.availableThrougtput b.availableThrougtput
        }


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


removeUnwantedItem : ActivatedAltRecipes -> ItemIO -> Bool
removeUnwantedItem options (ItemIO _ item) =
    if isOptionActivated IncludeResourceExtractor options then
        True

    else
        case item of
            LimeStone ->
                False

            IronOre ->
                False

            CopperOre ->
                False

            Water ->
                False

            Oil ->
                False

            Coal ->
                False

            _ ->
                True


addProductionNeeds : ActivatedAltRecipes -> ItemIO -> ProductionLane -> ( ProductionLane, List ItemIO )
addProductionNeeds options io lane =
    let
        itemName =
            intermediateProductText (getProduct io)
    in
    case Dict.get itemName lane of
        Nothing ->
            let
                group =
                    itemIOToMachineGroup options io
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
                            itemIOToMachineGroup options missingItemIO

                        groupToAdd =
                            addMachineGroup existingGroup newGroup
                    in
                    ( Dict.insert itemName groupToAdd lane, machineGroupInput newGroup )


getProductLaneFor : ByproductUsage -> ActivatedAltRecipes -> ProductionOptions -> List ItemIO -> ProductionLane
getProductLaneFor usage altRecipes options io =
    let
        lane =
            getProductLaneForHelper altRecipes options io Dict.empty

        -- TODO Merge instead of inserting
        insertMachineGroup group =
            Dict.insert (intermediateProductText <| machineGroupProduction group) group
    in
    getResiduesOfLane lane
        |> List.concatMap (splitResidueRecipe usage)
        |> List.foldl insertMachineGroup lane


getProductLaneForHelper : ActivatedAltRecipes -> ProductionOptions -> List ItemIO -> ProductionLane -> ProductionLane
getProductLaneForHelper altRecipes options remaining lane =
    case remaining of
        [] ->
            lane

        io :: rest ->
            let
                ( newLane, missingIO ) =
                    addProductionNeeds altRecipes io lane

                filteredMissingIO =
                    List.filter (removeUnwantedItem options) missingIO
            in
            getProductLaneForHelper altRecipes options (List.append rest filteredMissingIO) newLane


splitIO : Float -> ItemIO -> ( ItemIO, ItemIO )
splitIO coef (ItemIO quantity item) =
    ( ItemIO (coef * quantity) item
    , ItemIO ((1 - coef) * quantity) item
    )


getRecipesUsingResidue : IntermediateProduct -> List Recipe
getRecipesUsingResidue product =
    case product of
        PolymerResidue ->
            [ Refinery
                { input = ( perMinute 60 PolymerResidue, Just <| perMinute 20 Water )
                , output = perMinute 20 Plastic
                , byproduct = Nothing
                }
            , Refinery
                { input = ( perMinute 40 PolymerResidue, Just <| perMinute 40 Water )
                , output = perMinute 20 Rubber
                , byproduct = Nothing
                }
            ]

        HeavyOilResidue ->
            [ Refinery
                { input = ( perMinute 60 HeavyOilResidue, Nothing )
                , output = perMinute 40 Fuel
                , byproduct = Nothing
                }
            , Refinery
                { input = ( perMinute 40 HeavyOilResidue, Nothing )
                , output = perMinute 120 Coke
                , byproduct = Nothing
                }
            ]

        _ ->
            []


getResiduesOfLane : ProductionLane -> List ItemIO
getResiduesOfLane lane =
    Dict.values lane
        |> List.filterMap getMachineGroupByproduct


getMachineGroupByproduct : MachineGroup -> Maybe ItemIO
getMachineGroupByproduct (MachineGroup { what, count }) =
    recipeByproduct what
        |> Maybe.map (mulItemIO count)


getResidueRatio : IntermediateProduct -> ByproductUsage -> Maybe Float
getResidueRatio product usage =
    case usage of
        None ->
            Nothing

        Balanced { heavyOilResidueRatio, polymerResidueRatio } ->
            case product of
                HeavyOilResidue ->
                    Just heavyOilResidueRatio

                PolymerResidue ->
                    Just polymerResidueRatio

                _ ->
                    Nothing


splitResidueRecipe : ByproductUsage -> ItemIO -> List MachineGroup
splitResidueRecipe usage io =
    let
        residueRecipeToMachineGroup : ItemIO -> Recipe -> MachineGroup
        residueRecipeToMachineGroup (ItemIO targetQuantity _) recipe =
            let
                perMachineQuantity =
                    getQuantity (recipeOutput recipe)

                machineNeeded =
                    targetQuantity
                        / perMachineQuantity
                        |> ceiling

                -- TODO Calculate correct output ratio with incomplete input
                thougtput =
                    ItemIO 0 (getProduct <| recipeOutput recipe)
            in
            MachineGroup
                { what = recipe
                , count = machineNeeded
                , availableThrougtput = thougtput
                }
    in
    case getResidueRatio (getProduct io) usage of
        Nothing ->
            []

        Just ratio ->
            case getRecipesUsingResidue (getProduct io) of
                [ recipeA, recipeB ] ->
                    let
                        ( ioA, ioB ) =
                            splitIO ratio io
                    in
                    [ residueRecipeToMachineGroup ioA recipeA
                    , residueRecipeToMachineGroup ioB recipeB
                    ]

                _ ->
                    []


productLaneGraph : ProductionLane -> ProductionLaneGraph
productLaneGraph lane =
    let
        values =
            Dict.values lane

        createNode id group =
            { id = id, label = group }

        createEdge from to io =
            { from = from, to = to, label = io }

        getIOIndex : ItemIO -> Int
        getIOIndex io =
            listIndex (\group -> machineGroupProduction group == getProduct io) values
                |> Maybe.withDefault -1

        edgeMapper : Int -> MachineGroup -> List (Graph.Edge ItemIO)
        edgeMapper index group =
            machineGroupInput group
                |> List.map (\io -> createEdge (getIOIndex io) index io)

        nodes =
            List.indexedMap createNode values

        edges =
            List.indexedMap edgeMapper values
                |> List.concat
    in
    Graph.fromNodesAndEdges nodes edges


getLane : ProductGraphModel -> ProductionLane
getLane model =
    model.needs
        |> Array.toList
        |> getProductLaneFor model.byproductUsage model.altRecipes model.productionOptions



{- Encode -}


encodeGraph : ProductGraphModel -> E.Value
encodeGraph model =
    let
        graph =
            getLane model
                |> productLaneGraph

        createNode { id, label } =
            let
                title =
                    String.fromInt (machineGroupCount label) ++ " " ++ (intermediateProductText <| getProduct output) ++ " " ++ machineGroupMachine label

                output =
                    machineGroupOutput label
            in
            E.object
                [ ( "id", E.int id )
                , ( "title", E.string <| title )
                , ( "production", E.string <| intermediateProductText <| getProduct output )
                , ( "extra", E.string <| itemIOText <| machineGroupExtraneous label )

                -- , ( "fixedValue", E.float <| getQuantity <| output )
                , ( "color", E.string <| Color.toCssString <| productColor <| getProduct output )
                ]

        createLink { from, to, label } =
            E.object
                [ ( "source", E.int from )
                , ( "target", E.int to )
                , ( "value", E.float <| getQuantity label )
                , ( "type", E.string <| intermediateProductText <| getProduct label )
                , ( "title", E.string <| itemIOText label )
                ]
    in
    E.object
        [ ( "nodes"
          , E.list createNode <| Graph.nodes graph
          )
        , ( "links"
          , E.list createLink <| Graph.edges graph
          )
        ]



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

        Residue ->
            Debug.todo ""


itemIOText : ItemIO -> String
itemIOText (ItemIO num product) =
    String.fromFloat num
        ++ " "
        ++ intermediateProductText product


itemIOView : ItemIO -> Element msg
itemIOView (ItemIO float product) =
    let
        quantity =
            String.fromFloat float
    in
    row
        [ Element.spacing (space Ui.SmallSpace) ]
        [ text quantity, text <| intermediateProductText product ]


recipeTooltipView : Recipe -> Element msg
recipeTooltipView recipe =
    let
        consumption =
            recipeConsumption recipe
                |> mwView

        ioColumn : List ItemIO -> Element msg
        ioColumn list =
            column [] (List.map itemIOView list)
    in
    column
        [ Background.color <| color Ui.WhiteColor
        , Border.color <| color Ui.BlackColor
        , Border.rounded 5
        , Border.width 1
        , Element.padding <| space Ui.SmallSpace
        , Element.spacing <| space Ui.RegularSpace
        ]
        [ row []
            [ el [] (text "per machine:")
            , el [ Element.alignRight ] consumption
            ]
        , row [ Element.spacing <| space Ui.LargeSpace ]
            [ ioColumn <| recipeInput recipe
            , el [ Element.centerY ] (text "→")
            , ioColumn <| recipeOutputWithByroduct recipe
            ]
        ]


machineGroupView : MachineGroup -> Element msg
machineGroupView (MachineGroup { what, count, availableThrougtput }) =
    let
        remaining =
            if emptyItemIO availableThrougtput then
                Element.none

            else
                text <| "with a surplus of " ++ itemIOText availableThrougtput

        byproduct =
            case recipeByproduct what of
                Nothing ->
                    Element.none

                Just io ->
                    row [] [ text "and ", itemIOView io ]
    in
    row [ Element.spacing (space Ui.SmallSpace) ]
        [ text (String.fromInt count)
        , el [ Font.light, Ui.tooltip Element.above (recipeTooltipView what), Element.pointer ] (text (assemblyMachineText what))
        , text "producing"
        , el [ Font.light ] (text (intermediateProductText <| getProduct <| recipeOutput <| what))
        , remaining
        , byproduct
        ]


surround : a -> a -> List a -> List a
surround start end list =
    List.concat [ [ start ], list, [ end ] ]


productionLaneView : ProductionLane -> Element ProductGraphMsg
productionLaneView lane =
    let
        wrapGroupViewInLi group =
            row [] [ machineGroupView group ]

        addConsumption _ (MachineGroup { what, count }) mw =
            what
                |> recipeConsumption
                |> mulMW count
                |> addMW mw

        totalConsumption =
            lane
                |> Dict.foldl addConsumption (makeMW 0)

        neededGeneratorView ( generator, count ) =
            row
                [ Element.spacing (space Ui.SmallSpace) ]
                [ text (String.fromInt count), viewGenerator generator ]
    in
    column [ Element.spacing (space Ui.SmallSpace) ]
        [ row
            [ Element.spacing (space Ui.SmallSpace) ]
            [ text "Total consumption:"
            , mwView totalConsumption
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
                    [ Font.color (color Ui.DangerColor) ]
                    { onPress = Just (RemoveProductNeed index), label = text "Remove" }

            else
                Element.none
    in
    row [ Element.spacing (space Ui.SmallSpace) ]
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
                [ Font.color (color Ui.InfoColor) ]
                { onPress = Just AddProductNeed, label = text "+ Add" }
    in
    column [ Element.spacing (space Ui.SmallSpace) ]
        (List.concat
            [ [ text "I want to produce every minute" ]
            , needsInputs
            , [ addButton ]
            ]
        )


toggleAltRecipesView : ActivatedAltRecipes -> Element ProductGraphMsg
toggleAltRecipesView recipes =
    let
        checkbox ( key, value ) =
            Input.checkbox []
                { onChange = UpdateAltRecipe key
                , icon = Input.defaultCheckbox
                , checked = value
                , label = Input.labelRight [] (text key)
                }
    in
    column [ Element.spacing (space Ui.SmallSpace) ]
        [ text "Alt recipes"
        , column
            []
            (recipes |> Dict.toList |> List.map checkbox)
        ]


productionOptionsView : ProductionOptions -> Element ProductGraphMsg
productionOptionsView options =
    let
        checkbox ( key, value ) =
            Input.checkbox []
                { onChange = UpdateProductionOption key
                , icon = Input.defaultCheckbox
                , checked = value
                , label = Input.labelRight [] (text key)
                }
    in
    column [ Element.spacing (space Ui.SmallSpace) ]
        [ text "Production options"
        , column
            []
            (options |> Dict.toList |> List.map checkbox)
        ]


byproductUsageView : ByproductUsage -> Element ProductGraphMsg
byproductUsageView usage =
    let
        onChange bool =
            if bool then
                UpdateByProducts <| Balanced { heavyOilResidueRatio = 0.5, polymerResidueRatio = 0.5 }

            else
                UpdateByProducts <| None

        checked =
            case usage of
                None ->
                    False

                Balanced _ ->
                    True

        checkbox =
            Input.checkbox []
                { onChange = onChange
                , icon = Input.defaultCheckbox
                , checked = checked
                , label = Input.labelRight [] (text "Handle byproducts")
                }
    in
    column [ Element.spacing (space Ui.SmallSpace) ]
        [ text "Byproducts"
        , checkbox
        ]


viewProductGraph : ProductGraphModel -> Element ProductGraphMsg
viewProductGraph model =
    column [ Element.spacing (space Ui.RegularSpace), Element.width Element.fill ]
        [ row
            [ Element.spaceEvenly, Element.width Element.fill ]
            [ el [ Element.alignTop ] (editionLineView model)
            , el [ Element.alignTop ] (toggleAltRecipesView model.altRecipes)
            , el [ Element.alignTop ] (productionOptionsView model.productionOptions)
            , el [ Element.alignTop ] (byproductUsageView model.byproductUsage)
            ]
        , productionLaneView <| getLane model
        ]


viewGenerator : EnergyGenerator -> Element ProductGraphMsg
viewGenerator generator =
    case generator of
        BiomassBurner ->
            el
                [ Font.color (color Ui.GreenColor)
                , Font.bold
                ]
                (text "Biomass Burner")

        CoalGenerator ->
            el
                [ Font.color (color Ui.BlackColor)
                , Font.bold
                ]
                (text "Coal Generator")

        FuelGenerator ->
            el
                [ Font.color (color Ui.PurpleColor)
                , Font.bold
                ]
                (text "Fuel Generator")
