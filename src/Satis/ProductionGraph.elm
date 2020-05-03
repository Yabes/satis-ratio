module Satis.ProductionGraph exposing (ProductGraphModel, ProductGraphMsg, encodeGraph, initProductGraph, updateProductGraph, viewProductGraph)

import Array exposing (Array)
import Color
import Dict exposing (Dict)
import Element exposing (Element, column, el, row, text)
import Element.Font as Font
import Element.Input as Input
import Graph exposing (Graph)
import Html exposing (button, option, select)
import Html.Attributes exposing (selected, style, type_, value)
import Html.Events exposing (onInput)
import Json.Encode as E
import Option exposing (Options)
import Satis.Energy as Energy
import Satis.IO as IO exposing (ItemIO)
import Satis.MachineGroup as MachineGroup exposing (MachineGroup)
import Satis.Options.AlternateRecipe as AlternateRecipe exposing (AltRecipe)
import Satis.Options.Production as ProductionOptions exposing (ProductionOption(..))
import Satis.Product as Product exposing (IntermediateProduct(..))
import Satis.Recipe as Recipe exposing (Recipe)
import Ui exposing (color, space)
import Utils



{- msg -}


type ProductGraphMsg
    = SelectProduct Int IntermediateProduct
    | UpdateQuantity Int Float
    | AddProductNeed
    | RemoveProductNeed Int
    | UpdateAltRecipe AltRecipe Bool
    | UpdateProductionOption ProductionOption Bool
    | UpdateByProducts ByproductUsage


type ByproductUsage
    = None
    | Balanced { heavyOilResidueRatio : Float, polymerResidueRatio : Float }


type alias ProductionLane =
    Dict String MachineGroup


type alias ProductionLaneGraph =
    Graph MachineGroup ItemIO


type alias ProductGraphModel =
    { needs : Array ItemIO
    , altRecipes : Options AltRecipe
    , productionOptions : Options ProductionOption
    , byproductUsage : ByproductUsage
    }



{- Utils -}


initProductGraph : ProductGraphModel
initProductGraph =
    { needs = Array.fromList [ IO.perMinute 2 SmartPlating ]
    , altRecipes = AlternateRecipe.asOptions
    , productionOptions = ProductionOptions.asOptions
    , byproductUsage = None
    }


onProductUpdate : Int -> String -> ProductGraphMsg
onProductUpdate index product =
    product
        |> Product.parse
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
                            Array.set index (IO.setQuantity quantity io) model.needs
                    in
                    { model | needs = newNeeds }

        SelectProduct index product ->
            case Array.get index model.needs of
                Nothing ->
                    model

                Just io ->
                    let
                        newNeeds =
                            Array.set index (IO.setProduct product io) model.needs
                    in
                    { model | needs = newNeeds }

        AddProductNeed ->
            let
                newNeeds =
                    Array.push (IO.perMinute 1 IronPlate) model.needs
            in
            { model | needs = newNeeds }

        RemoveProductNeed index ->
            let
                newNeeds =
                    Utils.removeIndexInArray index model.needs
            in
            { model | needs = newNeeds }

        UpdateAltRecipe recipe bool ->
            { model | altRecipes = Option.set recipe bool model.altRecipes }

        UpdateProductionOption option bool ->
            { model | productionOptions = Option.set option bool model.productionOptions }

        UpdateByProducts newUsage ->
            { model | byproductUsage = newUsage }


removeUnwantedItem : Options ProductionOption -> ItemIO -> Bool
removeUnwantedItem options io =
    let
        item =
            IO.getProduct io
    in
    if Option.isActivated IncludeResourceExtractor options then
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


addProductionNeeds : Options AltRecipe -> ItemIO -> ProductionLane -> ( ProductionLane, List ItemIO )
addProductionNeeds options io lane =
    let
        itemName =
            Product.text <| IO.getProduct io
    in
    case Dict.get itemName lane of
        Nothing ->
            let
                group =
                    MachineGroup.itemIOToMachineGroup options io
            in
            ( Dict.insert itemName group lane, MachineGroup.input group )

        Just existingGroup ->
            case MachineGroup.consumeItems io existingGroup of
                MachineGroup.Sufficient mergedGroup ->
                    ( Dict.insert itemName mergedGroup lane, [] )

                MachineGroup.Insufficient fullGroup missingItemIO ->
                    ( Dict.insert itemName fullGroup lane, [ missingItemIO ] )

                MachineGroup.NoThrougtput missingItemIO ->
                    let
                        newGroup =
                            MachineGroup.itemIOToMachineGroup options missingItemIO

                        groupToAdd =
                            MachineGroup.add existingGroup newGroup
                    in
                    ( Dict.insert itemName groupToAdd lane, MachineGroup.input newGroup )


getProductLaneFor : ByproductUsage -> Options AltRecipe -> Options ProductionOption -> List ItemIO -> ProductionLane
getProductLaneFor usage altRecipes options io =
    let
        lane =
            getProductLaneForHelper altRecipes options io Dict.empty

        -- TODO Merge instead of inserting
        insertMachineGroup group =
            Dict.insert (Product.text <| MachineGroup.production group) group
    in
    getResiduesOfLane lane
        |> List.concatMap (splitResidueRecipe usage)
        |> List.foldl insertMachineGroup lane


getProductLaneForHelper : Options AltRecipe -> Options ProductionOption -> List ItemIO -> ProductionLane -> ProductionLane
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


getResiduesOfLane : ProductionLane -> List ItemIO
getResiduesOfLane lane =
    Dict.values lane
        |> List.filterMap MachineGroup.byproduct


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
        residueRecipeToMachineGroup innerIO recipe =
            let
                perMachineQuantity =
                    IO.getQuantity <| Recipe.output recipe

                machineNeeded =
                    IO.getQuantity innerIO
                        / perMachineQuantity
                        |> ceiling

                -- TODO Calculate correct output ratio with incomplete input
                thougtput =
                    IO.perMinute 0 <| IO.getProduct <| Recipe.output recipe
            in
            MachineGroup.make
                recipe
                machineNeeded
                thougtput
    in
    case getResidueRatio (IO.getProduct io) usage of
        Nothing ->
            []

        Just ratio ->
            case Recipe.getRecipesUsingResidue (IO.getProduct io) of
                [ recipeA, recipeB ] ->
                    let
                        ( ioA, ioB ) =
                            IO.split ratio io
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
            Utils.listIndex (\group -> MachineGroup.production group == IO.getProduct io) values
                |> Maybe.withDefault -1

        edgeMapper : Int -> MachineGroup -> List (Graph.Edge ItemIO)
        edgeMapper index group =
            MachineGroup.input group
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
                    String.fromInt (MachineGroup.count label) ++ " " ++ (Product.text <| IO.getProduct output) ++ " " ++ MachineGroup.machine label

                output =
                    MachineGroup.output label
            in
            E.object
                [ ( "id", E.int id )
                , ( "title", E.string <| title )
                , ( "production", E.string <| Product.text <| IO.getProduct output )
                , ( "extra", E.string <| IO.text <| MachineGroup.availableThrougtput label )

                -- , ( "fixedValue", E.float <| getQuantity <| output )
                , ( "color", E.string <| Color.toCssString <| Product.color <| IO.getProduct output )
                ]

        createLink { from, to, label } =
            E.object
                [ ( "source", E.int from )
                , ( "target", E.int to )
                , ( "value", E.float <| IO.getQuantity label )
                , ( "type", E.string <| Product.text <| IO.getProduct label )
                , ( "title", E.string <| IO.text label )
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


productionLaneView : ProductionLane -> Element ProductGraphMsg
productionLaneView lane =
    let
        wrapGroupViewInLi group =
            row [] [ MachineGroup.machineGroupView group ]

        totalConsumption =
            lane
                |> Dict.values
                |> List.map MachineGroup.consumption
                |> List.foldl Energy.add (Energy.make 0)

        neededGeneratorView ( generator, count ) =
            row
                [ Element.spacing (space Ui.SmallSpace) ]
                [ text (String.fromInt count), Energy.viewGenerator generator ]
    in
    column [ Element.spacing (space Ui.SmallSpace) ]
        [ row
            [ Element.spacing (space Ui.SmallSpace) ]
            [ text "Total consumption:"
            , Energy.viewMW totalConsumption
            , Energy.getNeededGenerator totalConsumption
                |> List.map neededGeneratorView
                |> List.intersperse (text " / ")
                |> Utils.surround (text "(") (text ")")
                |> row []
            ]
        , column
            []
            (lane
                |> Dict.values
                |> List.sortBy MachineGroup.order
                |> List.map wrapGroupViewInLi
            )
        ]


selectProductView : Int -> IntermediateProduct -> Element ProductGraphMsg
selectProductView index item =
    let
        productOptionView selectedProduct product =
            option
                [ value <| Product.text product
                , selected <| selectedProduct == product
                ]
                [ Html.text <| Product.text product ]

        inputSelect =
            Element.html <|
                select
                    [ onInput (onProductUpdate index), style "height" "40px" ]
                    (List.map (productOptionView item) Product.all)
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
itemIOInputView index io =
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
        [ updateProductQuantityView index (IO.getQuantity io)
        , selectProductView index (IO.getProduct io)
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


toggleAltRecipesView : Options AltRecipe -> Element ProductGraphMsg
toggleAltRecipesView recipes =
    let
        checkbox : ( AltRecipe, Bool ) -> Element ProductGraphMsg
        checkbox ( key, value ) =
            Input.checkbox []
                { onChange = UpdateAltRecipe key
                , icon = Input.defaultCheckbox
                , checked = value
                , label = Input.labelRight [] (text <| AlternateRecipe.text key)
                }
    in
    column [ Element.spacing (space Ui.SmallSpace) ]
        [ text "Alt recipes"
        , column
            []
            (recipes |> Option.toList |> List.map checkbox)
        ]


productionOptionsView : Options ProductionOption -> Element ProductGraphMsg
productionOptionsView options =
    let
        checkbox : ( ProductionOption, Bool ) -> Element ProductGraphMsg
        checkbox ( key, value ) =
            Input.checkbox []
                { onChange = UpdateProductionOption key
                , icon = Input.defaultCheckbox
                , checked = value
                , label = Input.labelRight [] (text <| ProductionOptions.text key)
                }
    in
    column [ Element.spacing (space Ui.SmallSpace) ]
        [ text "Production options"
        , column
            []
            (options |> Option.toList |> List.map checkbox)
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
