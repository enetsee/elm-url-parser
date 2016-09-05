module UrlPart exposing (..)

import Dict exposing (Dict)
import Dict


type alias UrlParts =
    { seen : List UrlPart
    , rest : List UrlPart
    }


type UrlPart
    = PathPart String
    | Query (Dict String String)
    | Param String String
    | Hash String


type alias UrlPartExtractor a =
    UrlPart -> Result String a


urlParts : List String -> List ( String, String ) -> String -> UrlParts
urlParts pathParts params hash =
    { seen = []
    , rest = (List.map PathPart pathParts) ++ [ Query (Dict.fromList params), Hash hash ]
    }


nextPart : UrlParts -> ( Maybe UrlPart, UrlParts )
nextPart parts =
    case parts.rest of
        x :: rest' ->
            ( Just x, { parts | rest = rest' } )

        _ ->
            ( Nothing, parts )


seePart : UrlPart -> UrlParts -> UrlParts
seePart part parts =
    { parts | seen = part :: parts.seen }


singlePart : UrlPart -> UrlParts
singlePart part =
    { seen = []
    , rest = [ part ]
    }


urlPartToString : UrlPart -> String
urlPartToString part =
    case part of
        PathPart xs ->
            xs

        Query ps ->
            Dict.toList ps
                |> List.map (\( k, v ) -> k ++ "=" ++ v)
                |> List.foldr (\acc x -> acc ++ "&" ++ x) ""

        Param k v ->
            k ++ "=" ++ v

        Hash xs ->
            xs


extractPathPart : UrlPartExtractor String
extractPathPart part =
    case part of
        PathPart x ->
            Ok x

        _ ->
            Err "Expected a path part."


extractHash : UrlPartExtractor String
extractHash part =
    case part of
        Hash x ->
            Ok x

        _ ->
            Err "Expected a hash fragment."


extractQuery : UrlPartExtractor (Dict String String)
extractQuery part =
    case part of
        Query dict ->
            Ok dict

        _ ->
            Err "Expected a query string."


extractParamValue : UrlPartExtractor String
extractParamValue part =
    case part of
        Param _ value ->
            Ok value

        _ ->
            Err "Expected a query parameter."


extractParamKey : UrlPartExtractor String
extractParamKey part =
    case part of
        Param key _ ->
            Ok key

        _ ->
            Err "Expected a query parameter."
