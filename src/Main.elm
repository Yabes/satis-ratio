module Main exposing (main)

import Browser
import Element
import Element.Font as Font
import Html exposing (Html)
import ProductDependencyGraph exposing (ProductGraphModel, ProductGraphMsg, initProductGraph, updateProductGraph, viewProductGraph)
import Stylesheet exposing (edges, stylesheetColor, stylesheetFontsize, stylesheetSpacing)



---- MODEL ----


type alias Model =
    { productGraph : ProductGraphModel
    }


init : ( Model, Cmd Msg )
init =
    ( { productGraph = initProductGraph
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ProductGraphMsg ProductGraphMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProductGraphMsg innerMsg ->
            ( { model | productGraph = updateProductGraph innerMsg model.productGraph }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { productGraph } =
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
            ]
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
