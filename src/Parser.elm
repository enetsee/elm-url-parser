module Parser exposing (Parser, urlParts, lit, custom, int, string, many, param, optParam, (</>), (<#>), (<?>), parseWith, parseWithDefault, formatPath, formatPart, oneOf)

{-|

# Change the URL
@docs Parser, urlParts, lit, string,int,many, custom, param, optParam, (</>), (<#>), (<?>), parseWith, parseWithDefault, formatPath, formatPart, oneOf

-}

import UrlPart exposing (..)
import String
import Dict exposing (Dict)
import Basics exposing (flip)


{-| A `Parser` is a way of turning a URL like `/blog/42/cat-herding-techniques`
into structured data.

The two type variables can be a bit tricky to understand. I think the best way
to proceed is to just start using it. You can go far if you just assume it will
do the intuitive thing.

**Note:** If you *insist* on digging deeper, I recommend figuring out the type
of `int </> int` based on the type signatures for `int` and `</>`. You may be
able to just know based on intuition, but instead, you should figure out
exactly how every type variable gets unified. It is pretty cool! From there,
maybe check out the implementation a bit.
-}
type Parser formatter result
    = Parser (UrlParts -> formatter -> Result String ( UrlParts, result ))


{-| Helper to construct `URLParts` from elements of a `location`

-}
urlParts : List String -> List ( String, String ) -> String -> UrlParts
urlParts pathParts params hash =
    { seen = []
    , rest = (List.map PathPart pathParts) ++ [ Query (Dict.fromList params), Hash hash ]
    }


apply : Parser a b -> UrlParts -> a -> Result String ( UrlParts, b )
apply (Parser s) parts input =
    s parts input


compose : Parser a b -> Parser b c -> Parser a c
compose p q =
    Parser <|
        \parts f ->
            case apply p parts f of
                Ok ( rest, g ) ->
                    apply q rest g

                Err msg ->
                    Err msg


{-| A combinator which causes the second parser to be applied to `PathPart`
`UrlPart`s only

-}
(</>) :
    (a -> Parser b c)
    -> (UrlPartExtractor String -> Parser c d)
    -> a
    -> Parser b d
(</>) toParser1 toParser2 urlPartToSomething =
    compose (toParser1 urlPartToSomething) (toParser2 extractPathPart)


{-| A combinator which causes the second parser to be applied to `Hash`
`UrlPart`s only

-}
(<#>) :
    (a -> Parser b c)
    -> (UrlPartExtractor String -> Parser c d)
    -> a
    -> Parser b d
(<#>) toParser1 toParser2 urlPartToSomething =
    compose (toParser1 urlPartToSomething) (toParser2 extractHash)


{-| A combinator which causes the second parser to be applied to `Query`
`UrlPart`s only

-}
(<?>) :
    (a -> Parser b c)
    -> (UrlPartExtractor String -> Parser c d)
    -> a
    -> Parser b d
(<?>) toParser1 toParser2 urlPartToSomething =
    compose (toParser1 urlPartToSomething) (toParser2 extractParamValue)


{-|
-}
parseWith :
    (UrlPartExtractor String -> Parser (b -> b) a)
    -> UrlParts
    -> Result String a
parseWith toParser chunks =
    Result.map snd <|
        apply (toParser extractPathPart) chunks identity


{-|
-}
parseWithDefault :
    a
    -> (UrlPartExtractor String -> Parser (b -> b) a)
    -> UrlParts
    -> a
parseWithDefault default parser =
    parseWith parser >> Result.withDefault default


{-|
-}
formatPart :
    (a -> b)
    -> (UrlPartExtractor b -> e)
    -> UrlPartExtractor a
    -> e
formatPart formatter toParser urlPartToSomething =
    toParser (urlPartToSomething >> resultBind (formatter >> Ok))


{-| -}
formatPath :
    a
    -> (UrlPartExtractor b -> Parser a c)
    -> UrlPartExtractor b
    -> Parser (c -> d) d
formatPath formatter toParser urlPartToSomething =
    Parser <|
        \parts g ->
            case apply (toParser urlPartToSomething) parts formatter of
                Ok ( rest, res ) ->
                    Ok ( rest, g res )

                Err msg ->
                    Err msg


oneOfHelp :
    List (UrlPartExtractor a -> Parser b c)
    -> UrlParts
    -> b
    -> UrlPartExtractor a
    -> Result String ( UrlParts, c )
oneOfHelp choices parts formatter urlPartToSomething =
    case choices of
        [] ->
            Err "Tried many parsers, but none of them worked!"

        toParser :: otherToParsers ->
            case apply (toParser urlPartToSomething) parts formatter of
                Err _ ->
                    oneOfHelp otherToParsers parts formatter urlPartToSomething

                Ok answerPair ->
                    Ok answerPair


{-| Try a bunch of parsers one at a time. This is useful when there is a known
set of branches that are possible. For example, maybe we have a website that
just has a blog and a search:

    type DesiredPage = Blog Int | Search String

    desiredPage : UrlPartExtractor String -> Parser (DesiredPage -> a) a
    desiredPage e =
      oneOf
        [ formatPath Blog (lit "blog" </> int)
        , formatPath Search (lit "search" </> string)
        ] e
The `desiredPage` parser will first try to match things like `/blog/42` and if
that fails it will try to match things like `/search/badgers`. It fails if none
of the parsers succeed.
-}
oneOf :
    List (UrlPartExtractor a -> Parser b c)
    -> UrlPartExtractor a
    -> Parser b c
oneOf choices urlPartToSomething =
    Parser <|
        \chunks formatter ->
            oneOfHelp choices chunks formatter urlPartToSomething


{-| A parser that matches *exactly* the given string. So the following parser
will match the URL `/hello/world` and nothing else:

    helloWorld : Parser a a
    helloWorld =
      lit "hello" </> lit "world"
-}
lit : String -> UrlPartExtractor String -> Parser a a
lit str urlPartToSomething =
    Parser <|
        \parts x ->
            case nextPart parts of
                ( Just part, rest ) ->
                    case urlPartToSomething part of
                        Ok p ->
                            if p == str then
                                Ok ( seePart part rest, x )
                            else
                                Err <| "Expected `" ++ str ++ "` but found `" ++ p ++ "`."

                        Err msg ->
                            Err msg

                _ ->
                    Err <| "End of url"


{-| Create a custom segment parser. The `int` and `string` parsers are actually
defined with it like this:

    import String

    string : UrlPartExtractor String -> Parser (String -> a) a
    string urlPartToSomething =
        custom "STRING" urlPartToSomething

    int : UrlPartExtractor String -> Parser (Int -> a) a
    int urlPartToSomething =
        custom "INT" (\part -> Result.andThen (urlPartToSomething part) String.toInt)

The first argument is to help with error messages. It lets us say something
like, &ldquo;Got to the end of the URL but wanted /STRING&rdquo; instead of
something totally nonspecific. The second argument lets you process the URL
segment however you want.

An example usage would be a parser that only accepts segments with a particular
file extension. So stuff like this:

    css : UrlPartExtractor String  -> Parser (String -> a) a
    css e =
      let f part =
        Result.andThen
          (e part)
          (\str ->
            if String.endsWith ".css" str then
              Ok str
            else
              Err "Need something that ends with .css"
          )
      in
      custom "FILE.css" f

-}
custom : String -> UrlPartExtractor a -> Parser (a -> b) b
custom tipe urlPartToSomething =
    Parser <|
        \parts f ->
            case nextPart parts of
                ( Just part, rest ) ->
                    case urlPartToSomething part of
                        Ok x ->
                            Ok ( seePart part rest, f x )

                        Err msg ->
                            Err <|
                                "Parsing `"
                                    ++ (urlPartToString part)
                                    ++ "` went wrong: "
                                    ++ msg

                _ ->
                    Err <| "Got to the end of the URL but wanted " ++ tipe


{-| A parser that matches any string. So the following parser will match
URLs like `/search/whatever` where `whatever` can be replaced by any string
you can imagine.
    search : Parser (String -> a) a
    search =
      s "search" </> string
**Note:** this parser will only match URLs with exactly two segments. So things
like `/search/this/that` would fail. You could use `search </> many string` to handle
that case if you wanted though!
-}
string : UrlPartExtractor String -> Parser (String -> a) a
string urlPartToSomething =
    custom "STRING" urlPartToSomething


{-| A parser that matches any integer. So the following parser will match
URLs like `/blog/42` where `42` can be replaced by any positive number.

    blog : UrlPartExtractor String -> Parser (Int -> a) a
    blog e =
      (lit "blog" </> int) e

**Note:** this parser will only match URLs with exactly two segments. So things
like `/blog/42/cat-herding-techniques` would fail. You could use `blog </> string`
to handle that scenario if you wanted though!
-}
int : UrlPartExtractor String -> Parser (Int -> a) a
int urlPartToSomething =
    custom "INT" (urlPartToSomething >> resultBind String.toInt)


manyHelper :
    (UrlPartExtractor a -> Parser (b -> b) c)
    -> UrlPartExtractor a
    -> List c
    -> UrlParts
    -> (List c -> d)
    -> Result e ( UrlParts, d )
manyHelper s e accu parts f =
    case nextPart parts of
        ( Just part, rest ) ->
            case apply (s e) (singlePart part) identity of
                Ok ( _, x ) ->
                    manyHelper s e (x :: accu) (seePart part rest) f

                _ ->
                    Ok ( parts, List.reverse accu |> f )

        _ ->
            Ok ( parts, List.reverse accu |> f )


{-| A parser that matches zero or more parts matching the underlying parserr.
So the following parser will match URLs like `/search/foo/bar` where `foo`
and `bar` can be replaced by any string you can imagine.

    search : UrlPartExtractor String  -> Parser (List String -> a) a
    search =
      lit "search" </> many string

-}
many :
    (UrlPartExtractor a -> Parser (b -> b) c)
    -> UrlPartExtractor a
    -> Parser (List c -> d) d
many s e =
    Parser <|
        manyHelper s e []


{-| A parser that matches a parameter with the specified `key` and a value
matching the supplied parser. The order of the parameters in the query string
does not matter.

    search : UrlPartExtractor String  -> Parser (String -> a) a
    search =
      lit "search" <?> param "x" string

**Note:** `param` must be used with the `<?>` combinator to ensure that a
`Query` `UrlPart` is extracted.
-}
param :
    String
    -> (UrlPartExtractor a -> Parser (b -> b) c)
    -> UrlPartExtractor a
    -> Parser (c -> d) d
param key parser urlPartToSomething =
    Parser <|
        \parts f ->
            case nextPart parts of
                ( Just (Query dict), rest ) ->
                    case tryFindRemove key dict of
                        ( Just v, dict' ) ->
                            let
                                rest' =
                                    if Dict.isEmpty dict' then
                                        rest
                                    else
                                        { rest | rest = (Query dict') :: rest.rest }

                                prm =
                                    Param key v
                            in
                                case apply (parser urlPartToSomething) (singlePart prm) identity of
                                    Ok ( _, r ) ->
                                        Ok ( seePart prm rest', f r )

                                    Err msg ->
                                        Err msg

                        _ ->
                            Err <| "Could not find parameter with key `" ++ key ++ "`."

                _ ->
                    Err "Expected a query string at this point in the URL."


{-| A parser that matches a parameter with the specified `key` and a value
matching the supplied parser which may or not be present.
The order of the parameters in the query string does not matter.

    search : UrlPartExtractor String  -> Parser (Maybe String -> a) a
    search =
      lit "search" <?> optParam "x" string

**Note:** `optParam` must be used with the `<?>` combinator to ensure that a
`Query` `UrlPart` is extracted.
-}
optParam :
    String
    -> (a -> Parser (b -> b) c)
    -> a
    -> Parser (Maybe c -> d) d
optParam key parser urlPartToSomething =
    Parser <|
        \parts f ->
            case nextPart parts of
                ( Just (Query dict), rest ) ->
                    case tryFindRemove key dict of
                        ( Just v, dict' ) ->
                            let
                                rest' =
                                    if Dict.isEmpty dict' then
                                        rest
                                    else
                                        { rest | rest = (Query dict') :: rest.rest }

                                prm =
                                    Param key v
                            in
                                case apply (parser urlPartToSomething) (singlePart prm) identity of
                                    Ok ( _, r ) ->
                                        Ok ( seePart prm rest', f (Just r) )

                                    Err msg ->
                                        Err msg

                        _ ->
                            Ok ( parts, f Nothing )

                _ ->
                    Ok ( parts, f Nothing )


tryFindRemove : comparable -> Dict comparable a -> ( Maybe a, Dict comparable a )
tryFindRemove key dict =
    case Dict.get key dict of
        Just v ->
            ( Just v, Dict.remove key dict )

        _ ->
            ( Nothing, dict )


resultBind : (a -> Result c b) -> Result c a -> Result c b
resultBind =
    flip Result.andThen
