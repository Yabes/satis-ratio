module Option exposing (Options, isActivated, make, set, toList)

import Dict exposing (Dict)



{- type -}


type Options a
    = Options
        { toKey : a -> String
        , data : Dict String ( a, Bool )
        }



{- utils -}


make : List a -> (a -> String) -> Options a
make all fn =
    let
        mapper : a -> ( String, ( a, Bool ) )
        mapper typedKey =
            ( fn typedKey, ( typedKey, False ) )

        data =
            List.map mapper all
                |> Dict.fromList
    in
    Options { toKey = fn, data = data }


isActivated : a -> Options a -> Bool
isActivated option (Options { toKey, data }) =
    Dict.get (toKey option) data
        |> Maybe.map Tuple.second
        |> Maybe.withDefault False


set : a -> Bool -> Options a -> Options a
set option value (Options { toKey, data }) =
    let
        newData =
            Dict.insert (toKey option) ( option, value ) data
    in
    Options { toKey = toKey, data = newData }


toList : Options a -> List ( a, Bool )
toList (Options { data }) =
    Dict.values data
