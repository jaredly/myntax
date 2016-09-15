
# Making a change to the core grammar type without too much fuss

1. packTypes.re - duplicate the `Parsing` module into a module named `NewParsing`
2. modify the types in `NewParsing` to meet your new needs
3. pack.re & grammarOfGrammar.re - change `let module P = PackTypes.Parsing` -> `NewParsing`
4. modify `grammarOfGrammar.re` to use the modified types
5. `make pack dump` - this will overwrite `grammarGrammar.re` with the new
  format
6. in grammarGrammar.re, replace all `NewParsing` with `Parsing`
7. remove the old Parsing module from `packTypes.re`, and rename `NewParsing`
   to `Parsing`
8. fix the various type errors that subsequently arise, probably in
   `packTypes.re`, `runtime.re`, etc.

# Things I've tried

lrtt parser -- can't remember what was bad; probably just old & unmaintained?

ohm - js, and awkward to use resutls. although awesome

teerex (from opalang) - doesn't support mutual left-recursion

https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/
- current try
- added (negative) lookahead
- no error messages though, want to add those
- also want grammar parsing, will work on that now
- also want the `lexical` vs `Nonlexical` distinction that ohm has. Ideal
  would be to be compatible w/ (a subset of) ohm's syntax



