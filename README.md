# maistissa-parser

Maistissa Parser reads wine tasting notes from text files, parses them and turns the result into JSON.

## Structure
The application consists of three modules:
1. A generic text parser.
2. A parser combinator that parses text files containing wine tasting notes.
3. A converter that turns the parsed Haskell records into JSON.

## References
The code for the parser and parser combinator is based on the following articles:

<ul>
  <li>https://serokell.io/blog/parser-combinators-in-haskell</li>
  <li>https://github.com/japiirainen/microparser</li>
  <li>https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell</li>
</ul>