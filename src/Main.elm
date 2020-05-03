port module Main exposing (main)

import Browser
import Element
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import Satis.ProductionGraph exposing (ProductGraphModel, ProductGraphMsg, encodeGraph, initProductGraph, updateProductGraph, viewProductGraph)
import Svg
import Svg.Attributes
import Ui exposing (color, fontsize, noEdges, space)



---- PORTS ----


port setDiagramData : E.Value -> Cmd msg



---- ENCODE/DECODE ----


encode : Model -> E.Value
encode model =
    encodeGraph model.productGraph


decode : D.Decoder GraphMeta
decode =
    D.map2 GraphMeta
        (D.field "width" D.int)
        (D.field "height" D.int)



---- MODEL ----


type alias GraphMeta =
    { width : Int, height : Int }


type alias Model =
    { productGraph : ProductGraphModel
    , graph : GraphMeta
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    let
        graph =
            case D.decodeValue decode flags of
                Ok graph_ ->
                    graph_

                Err _ ->
                    { width = 0, height = 0 }

        model =
            { productGraph = initProductGraph
            , graph = graph
            }
    in
    ( model
    , setDiagramData <| encode model
    )



---- UPDATE ----


type Msg
    = ProductGraphMsg ProductGraphMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProductGraphMsg innerMsg ->
            let
                newModel =
                    { model | productGraph = updateProductGraph innerMsg model.productGraph }
            in
            ( newModel
            , setDiagramData <| encode newModel
            )



---- VIEW ----


view : Model -> Html Msg
view { productGraph, graph } =
    Element.layout
        [ Font.color (color Ui.TextColor)
        , Font.family [ Font.monospace ]
        , Element.padding (space Ui.SmallSpace)
        ]
        (Element.column
            [ Font.size (fontsize Ui.TextSize) ]
            -- Title
            [ Element.el
                [ Font.size (fontsize Ui.TitleSize)
                , Element.paddingEach { noEdges | bottom = space Ui.LargeSpace }
                ]
                (Element.text "Blunt Satisfactory ratio calculator")

            -- Graph settings and text view
            , Element.map ProductGraphMsg (viewProductGraph productGraph)

            -- Sankey diagram
            , Element.el
                [ Element.padding <| space Ui.LargeSpace
                , Element.width <| Element.px graph.width
                , Element.height <| Element.px graph.height
                ]
                (Element.html <|
                    Svg.svg [ Svg.Attributes.id "sankey" ] []
                )
            ]
        )



---- PROGRAM ----


main : Program E.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
