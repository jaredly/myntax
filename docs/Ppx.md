# Defining a grammar

To streamline the creation of grammars and converters, I've included a ppx, called "Ppx_grammar", which implements a DSL that dramatically cuts down on the required boilerplate.
Any file in the `grammars` directory will be transformed.

If a module contains `%%rule`s a `start` function is exported, which takes a `~filename` (for error reporting) and a string to parse & convert. It will return whatever the `Start` rule's conversion function returns.

## Rules

Every rule needs a name, provided via the `[@name "SomeName"]` attribute.
The body of a rule must be one of three things:

- a string which defines the grammar of the rule
- a tuple of a grammar string and a conversion function
- a list of tuples, which are `(sub_name, grammar_string, conversion_function)`. The sub_names must be unique within a rule.

**Capitalization** indicates whether a node is "lexical" or not (whether whitespace should be skipped). Capital-named rules will skip whitespace between terms, whereas lower-cased-named rules will not.

So
```
[@name "someRule"]
[%%rule {|"one" "two"|}]
```
would *only* match `onetwo`, not `one two`. If the name were `SomeRule`, then `one two` would match.

## the Grammar format

The grammar string is made up of literals (surrounded in quotes) and references (which are not). References can have labels (surrounded by `[]`), and items can be grouped with parenthesis `()`. An item or a group can have the suffix `?`, `*`, or `+`, which allow 0 or 1, 0 or more, and 1 or more, respectively.

You can do a negative lookahead with a `~` prefix.

The grammar, defined in itself, looks something like:

```
Item = "~"? ("[" ident "]")? ItemInner suffix?
ItemInner =
  | string
  | ident
  | "(" NestedItems ")" -- nested
  | char_range
  | char
suffix =
  | "+" -- plus
  | "*" -- star
  | "?" -- opt
char_range = "'" single ".." single "'"
```

For more info, check out [the full grammar](../parsable/grammar). The starting rule for the grammar string is "Choice".

## Conversion functions

The conversion function is where lots of the magic happens. Annotations on the arguments indicate what conversion needs to be done.

- `[@node "SomeName"]thing` - get and convert the first `SomeName` node in the list. If there's no `SomeName`, this will throw.
- `[@node_opt "SomeName"]boop` - same as `node` but is an optional
- `[@text "someName"](text, loc)` - get the raw text and location associated with the **leaf** node "someName". Text is a string, and loc is a `Location.t` (from compiler-libs).

The return type of a conversion function is entirely up to you. Of course, the return types of various "branches" of a single rule must all return the same type.

An argument `[@node "SomeName"]thing` would have whatever type the `SomeName` conversion function returned.

If you have an argument `~loc`, it will be populated with the `Location.t` encompassing the current node.

## Decorators
A rule can have decorators that modify behavior.

#### `[@leaf]`

Leaf nodes in the parse tree are represented as a strings -- the node's children are not individually tracked. In parsers that have separate lexing and parsing passes, you can think of this as tokens that are interesting (like identifiers, operators, etc.) as opposed to keywords, etc.

Leaf rules do not have conversion functions.

#### `[@ignoreNewlines]` and `[@ignoreNewlines false]`

This property is inherited -- if a rule ignores newlines, all of its chidlren will ignore newlines, unless a rule explicitly turns ignorenewlines off, which will then apply to that rules children etc.

#### `[@passThrough]`

This indicates that this rule should **not** be retained in the parse tree. Its children will be included in the parents term list.

So
```
[@name "One"]
[%%rule "Two Three"];

[@passThrough]
[@name "Two"]
[%%rule "Four Five"];
```

If `One` was successfully parsed, its child list would contain the nodes `[Four, Five, Three]`.


## Example grammar
[source](../grammars/mathGrammar.re)

```re
[@name "Start"]
[%%rule (
  "Expression",
  ([@node "Expression"]expr) => expr
)];

[@name "Expression"]
[%%rule [(
  "add",
  {|[left]Expression "+" [right]Expression|},
  ([@node.left "Expression"]left, [@node.right "Expression"]right) => left + right
), (
  "sub",
  {|[left]Expression "-" [right]Expression|},
  ([@node.left "Expression"]left, [@node.right "Expression"]right) => left - right
), (
  "int", {|int|},
  ([@text "int"](int, _)) => int_of_string(int)
)]];

[@leaf]
[@name "int"]
[%%rule "digit+"];

[@name "digit"]
[%%rule "'0..9'"];
```
