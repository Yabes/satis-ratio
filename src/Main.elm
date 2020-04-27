port module Main exposing (main)

import Browser
import Element
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as D
import Json.Encode as E
import ProductDependencyGraph exposing (ProductGraphModel, ProductGraphMsg, encodeGraph, initProductGraph, updateProductGraph, viewProductGraph)
import Stylesheet exposing (edges, stylesheetColor, stylesheetFontsize, stylesheetSpacing)
import Svg
import Svg.Attributes



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
        [ Font.color (stylesheetColor Stylesheet.TextColor)
        , Font.family [ Font.monospace ]
        , Element.padding (stylesheetSpacing Stylesheet.SmallSpace)
        ]
        (Element.column
            [ Font.size (stylesheetFontsize Stylesheet.TextSize) ]
            [ Element.el
                [ Font.size (stylesheetFontsize Stylesheet.TitleSize)
                , Element.paddingEach { edges | bottom = stylesheetSpacing Stylesheet.LargeSpace }
                ]
                (Element.text "Blunt Satisfactory ratio calculator")
            , Element.map ProductGraphMsg (viewProductGraph productGraph)
            , Element.el
                [ Element.padding <| stylesheetSpacing Stylesheet.LargeSpace
                , Element.width <| Element.px graph.width
                , Element.height <| Element.px graph.height
                ]
                (Element.html <|
                    Svg.svg
                        [ Svg.Attributes.id "sankey"
                        ]
                        []
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
