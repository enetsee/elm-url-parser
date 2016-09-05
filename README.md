# elm-url-parser

This library is an attempt to add more functionality to `evancz/url-parser`. Specifically, the library has combinators to deal with query parameters and hash fragments:
- (</>) : composes two parsers with the second being applied to `PathPart`s only (see below)
- (<#>) : composes two parsers with the second being applied to `Hash`s only (see below)
- (<?>) : composes two parsers with the second being applied to `Query`s only (see below)

In addition, there are a couple other combinators:
- `many` : matches zero or more URL chunks which match the supplied parser
- `param` : matches a query parameter with the given key and a value matching the supplied parser
- `optParam`: matches a query parameter with the given key and a value matching the supplied parser which may or may not be present in the `location` `search`

(I'm using a `Dict String String` to hold the key-value pairs in the `location` `search` so you don't need to worry about the ordering of the query parameters when using the `param` and `optParam` combinators.)

## Approach

I have introduced an ADT (`UrlPart`) to differentiate between different parts of a URL. There are four data constructors:
- PathPart : an element of the `location` `pathname`
- Query : a dictionary containing the key-value pairs in the `location` `search`
- Hash : the `location` `hash`
- Param : an individual key-value pair in the `location` `search`

Only the first three of these are used for scanning a URL; `Param` is pushed into the `seen` list when it is parsed from the `Query` URL part.

## Problems

The types are *very* confusing. I have introduced a type alias `UrlPartExtractor a` (for `UrlPart -> Result String a`) which is used to determine which part of URL a parser should be applied to.

It feels like this could be moved into the definition of the `Parser` data type but I cannot figure out how.

Secondly, I am totally lost with the function `formatPart` and `formatPath`. In @evancz's library there is a single `format` function but I had to twiddle the types until I got the result I wanted. If the types can be lined up, I suspect these functions are unnecessary.

I would appreciate any thoughts or pointers on the additions.
