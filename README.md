# elm-markdown

This version the focus is on parsing lines one at a time, and then 
making structures afterwards.  
Trying to follow the commonmark parsing strategy.

# Dependencies
- [Parser](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1/) As the Parser library
- [elm-html-parser](http://package.elm-lang.org/packages/jinjor/elm-html-parser/1.1.5) A parser, which parses plain html, and outputs the corresponding elm html data type.

# Tests
For testing purposes the following packages are used:
- [elm-test](http://package.elm-lang.org/packages/elm-community/elm-test/4.2.0/) The main testing framework
- [html-test-runner](http://package.elm-lang.org/packages/elm-community/html-test-runner/1.0.7/) A supplement which generates a website to show test results
- [elm-html-test](http://package.elm-lang.org/packages/eeue56/elm-html-test/5.2.0/) A supplement to test aspects of generated Html - Since i parse to the Html type, this is quite useful.
