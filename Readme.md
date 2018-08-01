

# Grammar grammar


- rules that start with a lowercase letter are "lexical", meaning whitespace is *not* skipped.
- rules that start with an uppercase letter are non-lexical, so non-newline whitespace is skipped

### Annotations

#### @ignoreNewlines

If you annotate a rule with `@ignoreNewlines(true)`, then newlines will also be skipped *recursively* until it gets to a rule that has `@ignoreNewlines(false)`.

#### @passThrough

Non-literal children will be propagated up to the caller.

# TODO

- [ ] preserving newlines would be nice
- [ ] preserve comments pls
- [ ] make sure I'm hanging on to locations of things
- [ ] better error messages! I think I can add nice messages to a given rule item, detailing what the correct syntax should be. Like "in a record declaration, you can only do :keyword and :keyword int". Although maybe I can do that automatically? That would be super cool. Also I could generate "examples" automatically via the pretty printer. Would want to generate "minimally interesting" examples, that followed the longest branch that we were on.
- [ ] pretty printing could use a lot of love too. I want to be able to represent the "fill box" from Format. Also specify indentation groups, etc.


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



