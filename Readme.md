
# Myntax
A parser, converter, and pretty-printer generator for OCaml.

Based on https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/, with heavy modifications.

- [Defining a Grammar](docs/Grammar.md)
- [The Lisp.re grammar generated documentation](docs/LispGrammar.md)


# Lisp.re

Building & using the lisp-to-ocaml parser

- clone this repo
- `npm install`
- `npm run build`
- `./lib/bs/native/lisp.native ml some-file.rel` will print out the lisp.re file converted to ocaml syntax
- `./lib/bs/native/lisp.native bin some-file.rel > some-file.ast` will act as a `-pp`, it will output the binary representation of the ocaml AST to standard out (here redirected into a file). You can then compile it like `ocamlc -impl some-file.ast`.

## Using Lisp.re in a real project

The ocaml compiler actually makes this super easy :D.
You can set a up a watcher that, whenever a .rel file changes, will run `lisp.native bin that-file.rel > that-file.ml`. Then, bsb or jbuilder will see the `.ml` file and process it, smoothly handling the fact that it's already been parsed. This way token location information is preserved!

# TODO

- [ ] preserving newlines would be nice
- [ ] preserve comments pls
- [ ] make sure I'm hanging on to locations of things
- [ ] better error messages! I think I can add nice messages to a given rule item, detailing what the correct syntax should be. Like "in a record declaration, you can only do :keyword and :keyword int". Although maybe I can do that automatically? That would be super cool. Also I could generate "examples" automatically via the pretty printer. Would want to generate "minimally interesting" examples, that followed the longest branch that we were on.
- [ ] pretty printing could use a lot of love too. I want to be able to represent the "fill box" from Format. Also specify indentation groups, etc.



