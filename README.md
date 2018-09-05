# unuhi

Parsing Typeclasses for cats and scala (this could potentially graduate into a cats-parsing project). This is at a very early stage and has yet to publish any artifacts.

The main goals are:

1. define ParserA an Alternative with Delay with some additional text parsing functions very similar to [Haskell's Text.Parser.Parsing](https://hackage.haskell.org/package/parsers-0.12.9/docs/Text-Parser-Combinators.html#g:2) typeclass.
2. define ParserM a Monad with ParserA for monadic parsing.
3. write some reusable laws for these typeclasses
4. write instances of these typeclasses for commonly used scala parsing libraries
5. give an internal implementations as examples.
6. provide some example parsers using only the typeclass which are suitable for running with any instance (e.g. Json example).
