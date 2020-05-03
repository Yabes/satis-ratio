module Utils exposing (listFind, listIndex, removeIndexInArray, surround)

import Array exposing (Array)


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


removeIndexInArray : Int -> Array a -> Array a
removeIndexInArray index array =
    let
        before =
            Array.slice 0 index array

        after =
            Array.slice (index + 1) (Array.length array) array
    in
    Array.append before after


surround : a -> a -> List a -> List a
surround start end list =
    List.concat [ [ start ], list, [ end ] ]
