module Satis.IO exposing (ItemIO, add, canHandle, diminish, getProduct, getQuantity, isEmpty, mul, perMinute, setProduct, setQuantity, split, text, view)

import Element exposing (Element)
import Satis.Product as Product exposing (IntermediateProduct)
import Ui


type ItemIO
    = ItemIO Float IntermediateProduct



{- Utils -}
-- TODO encure we're not adding 2 different kind of item together


add : ItemIO -> ItemIO -> ItemIO
add (ItemIO a item) (ItemIO b _) =
    ItemIO (a + b) item


mul : Int -> ItemIO -> ItemIO
mul coef (ItemIO quantity item) =
    let
        floatCoef =
            toFloat coef
    in
    ItemIO (floatCoef * quantity) item


isEmpty : ItemIO -> Bool
isEmpty (ItemIO quantity _) =
    quantity <= 0


canHandle : Float -> ItemIO -> Bool
canHandle target (ItemIO quantity _) =
    quantity >= target


diminish : Float -> ItemIO -> ItemIO
diminish toRemove (ItemIO quantity item) =
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


split : Float -> ItemIO -> ( ItemIO, ItemIO )
split coef (ItemIO quantity item) =
    ( ItemIO (coef * quantity) item
    , ItemIO ((1 - coef) * quantity) item
    )


perMinute : Float -> IntermediateProduct -> ItemIO
perMinute quantity item =
    ItemIO quantity item



{- view -}


text : ItemIO -> String
text (ItemIO num product) =
    String.fromFloat num
        ++ " "
        ++ Product.text product


view : ItemIO -> Element msg
view (ItemIO float product) =
    let
        quantity =
            String.fromFloat float
    in
    Element.row
        [ Element.spacing (Ui.space Ui.SmallSpace) ]
        [ Element.text quantity, Element.text <| Product.text product ]
