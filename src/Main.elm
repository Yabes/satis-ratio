module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, style)
import ProductDependencyGraph exposing (ProductGraphModel, ProductGraphMsg, initProductGraph, updateProductGraph, viewProductGraph)



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
    div [ style "font-family" "monospace", style "margin" "8px" ]
        [ h1 [] [ text "Blunt Satisfactory ratio calculator" ]
        , Html.map ProductGraphMsg (viewProductGraph productGraph)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
