
# Myntax
A parser, converter, and pretty-printer generator for OCaml.

Based on https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/, with heavy modifications.

- [Defining a Grammar](docs/Grammar.md)
- [The Lisp.re grammar generated documentation](docs/LispGrammar.md)

# Contributing

- `npm install`
- `npm run build` (you'll have to run this a couple times before it compiles without error 🙃)
- `cd reason-lisp`
- `npm install` (this will fail)
- `cd node_modules; rm -rf myntax; ln -s ../../ myntax; cd ..`
- `npm run build`

yeah it's pretty hacky. I think I might just switch over to esy+dune soon


# Lisp.re
NOTE: this is all out of date

Building & using the lisp-to-ocaml parser

- clone this repo
- `npm install`
- `npm run build`
- `./lib/bs/native/lisp.native ml some-file.rel` will print out the lisp.re file converted to ocaml syntax
- `./lib/bs/native/lisp.native bin some-file.rel > some-file.ast` will act as a `-pp`, it will output the binary representation of the ocaml AST to standard out (here redirected into a file). You can then compile it like `ocamlc -impl some-file.ast`.

## Using Lisp.re in a real project

The ocaml compiler actually makes this super easy :D.

`watch.js` is a script that, when run with a directory, will watch the `.rel` files in that directory, and recompile them when they change, piping the binary output into the equivalent `.ml` file. You can then have bucklescript watching the directory, or jbuilder running, and they will treat the binary `.ml` file as a perfectly normal source file.

`npm run example` will start the watcher for `example/src`. You can then start bsb in the example directory with `cd example && npm start`, and edit `example/src/Hello.rel` and see bsb recompile.

# TODO

## Parsing

- [ ] better error messages! I think I can add nice messages to a given rule item, detailing what the correct syntax should be. Like "in a record declaration, you can only do :keyword and :keyword int". Although maybe I can do that automatically? That would be super cool. Also I could generate "examples" automatically via the pretty printer. Would want to generate "minimally interesting" examples, that followed the longest branch that we were on.

## Pretty-printing

- [ ] preserving newlines between forms would be nice
- [ ] preserve comments pls
- [ ] I want to be able to represent the "fill box" from Format. Also specify indentation groups, etc.



