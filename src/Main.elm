module Main exposing (main)

import Browser
import Element
import Element.Font as Font
import Html exposing (Html)
import ProductDependencyGraph exposing (ProductGraphModel, ProductGraphMsg, initProductGraph, updateProductGraph, viewProductGraph)
import Style exposing (edges, stylesheetColor, stylesheetFontsize, stylesheetSpacing)



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
        [ Font.color (stylesheetColor Style.PrimaryColor)
        , Font.family [ Font.monospace ]
        , Element.padding (stylesheetSpacing Style.SmallSpace)
        ]
        (Element.column
            [ Font.size (stylesheetFontsize Style.TextSize) ]
            [ Element.el
                [ Font.size (stylesheetFontsize Style.TitleSize)
                , Element.paddingEach { edges | bottom = stylesheetSpacing Style.LargeSpace }
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
