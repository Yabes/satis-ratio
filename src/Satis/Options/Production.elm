module Satis.Options.Production exposing (ProductionOption(..), all, asOptions, text)

import Option exposing (Options)



{- type -}


type ProductionOption
    = IncludeResourceExtractor



{- utils -}


all : List ProductionOption
all =
    [ IncludeResourceExtractor ]


text : ProductionOption -> String
text option =
    case option of
        IncludeResourceExtractor ->
            "Include Resource Extractor"


asOptions : Options ProductionOption
asOptions =
    Option.make all text
