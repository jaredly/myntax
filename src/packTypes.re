let module Result = {
  type resultType =
    | Terminal string
    | Lexical string string
    | Iter
    | Nonlexical string [@@deriving yojson];

  type result = {
    start: int,
    cend: int,
    typ: resultType,
    label: option string,
    children: list result,
  } [@@deriving yojson];
};
include Result;

let module Parsing = {
  type grammar = list production[@@deriving show]
  and production = (string, list choice)[@@deriving show] /* rule name -> e1 | e2 | ... */
  and choice = (string, string, list parsing)[@@deriving show] /* choice name, comment, sequence */
  and parsing =
    | Star parsing (option string)      /* e* */
    | Plus parsing (option string)      /* e+ */
    | Optional parsing (option string)  /* e? */
    | Any (option string) /* any */
    | EOF /* EOF */
    | Group (list parsing)  /* ( e ... ) */
    | Lookahead parsing  /* &e */
    | Not parsing        /* !e */
    | Lexify parsing     /* # somelexrule */
    | NonTerminal string (option string)/* nonterminal 'name' */
    | Terminal string (option string)   /* terminal */
    | Chars char char (option string)   /* [a-z] */
    | Empty              /* epsilon */[@@deriving show];
};
include Parsing;
