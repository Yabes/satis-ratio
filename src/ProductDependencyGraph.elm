module ProductDependencyGraph exposing (ProductGraphModel, ProductGraphMsg, initProductGraph, updateProductGraph, viewProductGraph)

import Array exposing (Array)
import Dict exposing (Dict)
import Element exposing (Element, column, el, row, text)
import Element.Font as Font
import Element.Input as Input
import Html exposing (button, option, select)
import Html.Attributes exposing (selected, style, type_, value)
import Html.Events exposing (onInput)
import List
import Stylesheet exposing (stylesheetColor, stylesheetSpacing)



{- Types -}


type RecipesOption
    = BoltedModularFrame
    | QuickwireStator
    | FusedWire
    | CopperRotor
    | IncludeMiner
    | SteamedCopperSheet


type ProductGraphMsg
    = SelectProduct Int IntermediateProduct
    | UpdateQuantity Int Float
    | AddProductNeed
    | RemoveProductNeed Int
    | UpdateAltRecipe String Bool


type EnergyGenerator
    = BiomassBurner
    | CoalGenerator
    | FuelGenerator


type AssemblyMachine
    = Miner
    | Smelter
    | Constructor
    | Assembler
    | WaterExtractor
    | OilExtractor
    | Foundry
    | Refinery


type Recipe
    = Recipe
        { machine : AssemblyMachine
        , input : List ItemIO
        , output : ItemIO
        , byproduct : Maybe ItemIO
        }



-- type AssemblyMachineV2
--     = -- Extractor
--       Miner { output : ItemIO }
--     | WaterExtractor { output : ItemIO }
--       -- Furnaces
--     | Smelter { input : ItemIO, output : ItemIO }
--     | Foundry { input : ( ItemIO, ItemIO ), output : ItemIO }
--       -- Transformation
--     | Constructor { input : ItemIO, output : ItemIO }
--     | Assembler { input : ( ItemIO, ItemIO ), output : ItemIO }
--     | Refinery { input : ( ItemIO, Maybe ItemIO ), output : ItemIO, byproduct : Maybe ItemIO }
--     | Residue
--
--
-- type MachineGroupV2
--     = MachineGroupV2 { what : AssemblyMachineV2, count : Int, availableThrougtput : ItemIO }
-- machineGroupInput : MachineGroupV2 -> List ItemIO
-- machineGroupProduction : MachineGroupV2 -> IntermediateProduct


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



-- Merge to principle of machine with the recipe to limit io ?


type MachineGroup
    = MachineGroup
        { kind : AssemblyMachine
        , count : Int
        , availableThrougtput : ItemIO
        , producing : IntermediateProduct
        , inputPerMachine : List ItemIO
        }


type alias ProductionLane =
    Dict String MachineGroup


type alias RecipesOptions =
    Dict String Bool


type alias ProductGraphModel =
    { needs : Array ItemIO
    , altRecipes : RecipesOptions
    }


allProducts : List IntermediateProduct
allProducts =
    [ CopperWire, CopperCable, CopperSheet, IronPlate, IronRod, Screw, ReinforcedIronPlate, Rotor, ModularFrame, SmartPlating, Steel, SteelBeam, SteelPipe, IndustrialSteelBeam, Stator, Motor, VersatileFramework, AutomaticWire, Plastic, Rubber, Fuel ]


allOptions : List RecipesOption
allOptions =
    [ BoltedModularFrame, IncludeMiner, QuickwireStator, FusedWire, CopperRotor, SteamedCopperSheet ]



{- Utils -}


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


altRecipeText : RecipesOption -> String
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

        IncludeMiner ->
            "Include Miners"

        SteamedCopperSheet ->
            "Steamed Copper Sheet"


initProductGraph : ProductGraphModel
initProductGraph =
    { needs = Array.fromList [ perMinute 2 SmartPlating ]
    , altRecipes = allOptions |> List.map (\key -> ( altRecipeText key, False )) |> Dict.fromList
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


machineConsumption : AssemblyMachine -> MW
machineConsumption machine =
    case machine of
        Assembler ->
            MW 15

        Constructor ->
            MW 4

        Smelter ->
            MW 12

        WaterExtractor ->
            MW 20

        Miner ->
            MW 5

        Foundry ->
            MW 16

        Refinery ->
            MW 30

        OilExtractor ->
            MW 40


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


itemIOToMachineGroup : RecipesOptions -> ItemIO -> MachineGroup
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


getMachingeGroupOrderIndex : MachineGroup -> Int
getMachingeGroupOrderIndex (MachineGroup { kind }) =
    case kind of
        Miner ->
            10

        WaterExtractor ->
            11

        OilExtractor ->
            12

        Smelter ->
            20

        Foundry ->
            21

        Refinery ->
            22

        Constructor ->
            30

        Assembler ->
            31


isOptionActivated : RecipesOption -> RecipesOptions -> Bool
isOptionActivated recipe recipes =
    let
        key =
            altRecipeText recipe
    in
    recipes
        |> Dict.get key
        |> Maybe.withDefault False


getRecipeOf : RecipesOptions -> IntermediateProduct -> Recipe
getRecipeOf options product =
    case product of
        IronOre ->
            Recipe
                { machine = Miner
                , input = []
                , output = perMinute 60 IronOre
                , byproduct = Nothing
                }

        CopperOre ->
            Recipe
                { machine = Miner
                , input = []
                , output = perMinute 60 CopperOre
                , byproduct = Nothing
                }

        CateriumOre ->
            Recipe
                { machine = Miner
                , input = []
                , output = perMinute 60 CateriumOre
                , byproduct = Nothing
                }

        Coal ->
            Recipe
                { machine = Miner
                , input = []
                , output = perMinute 60 Coal
                , byproduct = Nothing
                }

        Water ->
            Recipe
                { machine = WaterExtractor
                , input = []
                , output = perMinute 120 Water
                , byproduct = Nothing
                }

        LimeStone ->
            Recipe
                { machine = Miner
                , input = []
                , output = perMinute 60 LimeStone
                , byproduct = Nothing
                }

        Concrete ->
            if isOptionActivated IncludeMiner options then
                Recipe
                    { machine = Constructor
                    , input = [ perMinute 45 LimeStone ]
                    , output = perMinute 15 Concrete
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Constructor
                    , input = []
                    , output = perMinute 15 Concrete
                    , byproduct = Nothing
                    }

        CateriumIngot ->
            Recipe
                { machine = Smelter
                , input = [ perMinute 45 CateriumOre ]
                , output = perMinute 15 CopperIngot
                , byproduct = Nothing
                }

        Quickwire ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 12 CateriumIngot ]
                , output = perMinute 60 Quickwire
                , byproduct = Nothing
                }

        CopperWire ->
            if isOptionActivated FusedWire options then
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 70 CopperIngot, perMinute 7.5 CateriumIngot ]
                    , output = perMinute 225 CopperWire
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Constructor
                    , input = [ perMinute 15 CopperIngot ]
                    , output = perMinute 30 CopperWire
                    , byproduct = Nothing
                    }

        CopperSheet ->
            if isOptionActivated SteamedCopperSheet options then
                Recipe
                    { machine = Refinery
                    , input = [ perMinute 27.5 Water, perMinute 27.5 CopperIngot ]
                    , output = perMinute 27.5 CopperSheet
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Constructor
                    , input = [ perMinute 20 CopperIngot ]
                    , output = perMinute 10 CopperSheet
                    , byproduct = Nothing
                    }

        CopperCable ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 60 CopperWire ]
                , output = perMinute 30 CopperCable
                , byproduct = Nothing
                }

        IronPlate ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 30 IronIngot ]
                , output = perMinute 20 IronPlate
                , byproduct = Nothing
                }

        IronRod ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 15 IronIngot ]
                , output = perMinute 15 IronRod
                , byproduct = Nothing
                }

        Screw ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 10 IronRod ]
                , output = perMinute 40 Screw
                , byproduct = Nothing
                }

        ReinforcedIronPlate ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 30 IronPlate, perMinute 60 Screw ]
                , output = perMinute 5 ReinforcedIronPlate
                , byproduct = Nothing
                }

        Rotor ->
            if isOptionActivated CopperRotor options then
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 56.25 CopperSheet, perMinute 487.5 Screw ]
                    , output = perMinute 28.1 Rotor
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 20 IronRod, perMinute 100 Screw ]
                    , output = perMinute 4 Rotor
                    , byproduct = Nothing
                    }

        SmartPlating ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 2 ReinforcedIronPlate, perMinute 2 Rotor ]
                , output = perMinute 2 SmartPlating
                , byproduct = Nothing
                }

        ModularFrame ->
            if isOptionActivated BoltedModularFrame options then
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 7.5 ReinforcedIronPlate, perMinute 140 Screw ]
                    , output = perMinute 5 ModularFrame
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 3 ReinforcedIronPlate, perMinute 12 IronRod ]
                    , output = perMinute 2 ModularFrame
                    , byproduct = Nothing
                    }

        Steel ->
            if isOptionActivated IncludeMiner options then
                Recipe
                    { machine = Foundry
                    , input = [ perMinute 45 IronOre, perMinute 45 Coal ]
                    , output = perMinute 45 Steel
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Foundry
                    , input = []
                    , output = perMinute 45 Steel
                    , byproduct = Nothing
                    }

        SteelPipe ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 30 Steel ]
                , output = perMinute 20 SteelPipe
                , byproduct = Nothing
                }

        SteelBeam ->
            Recipe
                { machine = Constructor
                , input = [ perMinute 60 Steel ]
                , output = perMinute 15 SteelBeam
                , byproduct = Nothing
                }

        IndustrialSteelBeam ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 24 Steel, perMinute 30 Concrete ]
                , output = perMinute 6 IndustrialSteelBeam
                , byproduct = Nothing
                }

        VersatileFramework ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 2.5 ModularFrame, perMinute 30 SteelBeam ]
                , output = perMinute 5 VersatileFramework
                , byproduct = Nothing
                }

        AutomaticWire ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 2.5 Stator, perMinute 50 CopperWire ]
                , output = perMinute 2.5 AutomaticWire
                , byproduct = Nothing
                }

        Stator ->
            if isOptionActivated QuickwireStator options then
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 40 SteelPipe, perMinute 150 Quickwire ]
                    , output = perMinute 20 Stator
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Assembler
                    , input = [ perMinute 15 SteelPipe, perMinute 8 CopperWire ]
                    , output = perMinute 5 Stator
                    , byproduct = Nothing
                    }

        Motor ->
            Recipe
                { machine = Assembler
                , input = [ perMinute 10 Rotor, perMinute 10 Stator ]
                , output = perMinute 5 Motor
                , byproduct = Nothing
                }

        IronIngot ->
            if isOptionActivated IncludeMiner options then
                Recipe
                    { machine = Smelter
                    , input = [ perMinute 30 IronOre ]
                    , output = perMinute 30 IronIngot
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Smelter
                    , input = []
                    , output = perMinute 30 IronIngot
                    , byproduct = Nothing
                    }

        CopperIngot ->
            if isOptionActivated IncludeMiner options then
                Recipe
                    { machine = Smelter
                    , input = [ perMinute 30 CopperOre ]
                    , output = perMinute 30 CopperIngot
                    , byproduct = Nothing
                    }

            else
                Recipe
                    { machine = Smelter
                    , input = []
                    , output = perMinute 30 CopperIngot
                    , byproduct = Nothing
                    }

        Oil ->
            Recipe
                { machine = OilExtractor
                , input = []
                , output = perMinute 120 Oil
                , byproduct = Nothing
                }

        -- RFuel 60R -> 40
        Fuel ->
            Recipe
                { machine = Refinery
                , input = [ perMinute 60 Oil ]
                , output = perMinute 40 Fuel
                , byproduct = Just (perMinute 30 PolymerResidue)
                }

        -- RPlastic 60PR 20W -> 20
        Plastic ->
            Recipe
                { machine = Refinery
                , input = [ perMinute 30 Oil ]
                , output = perMinute 20 Plastic
                , byproduct = Just (perMinute 10 PolymerResidue)
                }

        -- RRubber 40PR 40W 20
        Rubber ->
            Recipe
                { machine = Refinery
                , input = [ perMinute 30 Oil ]
                , output = perMinute 20 Rubber
                , byproduct = Just (perMinute 20 PolymerResidue)
                }

        Coke ->
            Recipe
                { machine = Refinery
                , input = [ perMinute 40 PolymerResidue ]
                , output = perMinute 120 Coke
                , byproduct = Nothing
                }

        HeavyOilResidue ->
            Recipe { machine = Refinery, input = [], output = perMinute 0 HeavyOilResidue, byproduct = Nothing }

        PolymerResidue ->
            Recipe { machine = Refinery, input = [], output = perMinute 0 PolymerResidue, byproduct = Nothing }


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


addProductionNeeds : RecipesOptions -> ItemIO -> ProductionLane -> ( ProductionLane, List ItemIO )
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


getProductLaneFor : RecipesOptions -> List ItemIO -> ProductionLane
getProductLaneFor options io =
    getProductLaneForHelper options io Dict.empty


getProductLaneForHelper : RecipesOptions -> List ItemIO -> ProductionLane -> ProductionLane
getProductLaneForHelper options remaining lane =
    case remaining of
        [] ->
            lane

        io :: rest ->
            let
                ( newLane, missingIO ) =
                    addProductionNeeds options io lane
            in
            getProductLaneForHelper options (List.append rest missingIO) newLane



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


assemblyMachineText : AssemblyMachine -> String
assemblyMachineText machine =
    case machine of
        Constructor ->
            "Constructor"

        Assembler ->
            "Assembler"

        Miner ->
            "Miner"

        WaterExtractor ->
            "Pump"

        Smelter ->
            "Smelter"

        Foundry ->
            "Foundry"

        Refinery ->
            "Refinery"

        OilExtractor ->
            "Oil Extractor"


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
                "with a surplus of " ++ itemIOText availableThrougtput
    in
    row [ Element.spacing (stylesheetSpacing Stylesheet.SmallSpace) ]
        [ text (String.fromInt count)
        , el [ Font.light ] (text (assemblyMachineText kind))
        , text "producing"
        , el [ Font.light ] (text (intermediateProductText producing))
        , text remaining
        ]


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
    column [ Element.spacing (stylesheetSpacing Stylesheet.SmallSpace) ]
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


toggleOptionsView : RecipesOptions -> Element ProductGraphMsg
toggleOptionsView recipes =
    let
        altRecipeView ( key, value ) =
            Input.checkbox []
                { onChange = UpdateAltRecipe key
                , icon = Input.defaultCheckbox
                , checked = value
                , label = Input.labelRight [] (text key)
                }
    in
    column [ Element.spacing (stylesheetSpacing Stylesheet.SmallSpace) ]
        [ text "Options"
        , column
            []
            (recipes |> Dict.toList |> List.map altRecipeView)
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
        [ row
            [ Element.spaceEvenly, Element.width Element.fill ]
            [ editionLineView model
            , el [ Element.alignTop ] (toggleOptionsView model.altRecipes)
            ]
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

        FuelGenerator ->
            el
                [ Font.color (stylesheetColor Stylesheet.PurpleColor)
                , Font.bold
                ]
                (text "Fuel Generator")
