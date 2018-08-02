
module Parsing = {
  [@deriving show]
  type ignoreNewlines =
    | Yes
    | No
    | Inherit;
  type grammar = {
    lineComment: option(string),
    blockComment: option((string, string)),
    rules: list((string, rule))
  }
  and rule = {
    passThrough: bool,
    ignoreNewlines,
    leaf: bool,
    docs: option(string),
    choices: list(choice)
  }
  /* rule name -> e1 | e2 | ... */
  /* -> add "pass-through" and "ignore-whitespace" */
  and choice = (string, string, list(parsing)) /* choice name, comment, sequence */
  [@deriving show]
  and parsing =
    | Star(parsing) /* e* */
    | Plus(parsing) /* e+ */
    | Optional(parsing) /* e? */
    | Any(option(string)) /* any */
    | NoSpaceAfter(parsing) /* a printing rule, suppresses space */
    | NoSpaceBefore(parsing) /* a printing rule, suppresses space */
    | EOF /* EOF */
    | CommentEOL
    | Group(list(parsing)) /* ( e ... ) */
    | Lookahead(parsing) /* &e */
    | Not(parsing) /* !e */
    | Lexify(parsing) /* # somelexrule */
    | NonTerminal(string, option(string)) /* nonterminal 'name' */
    | Terminal(string, option(string)) /* terminal */
    | Chars(char, char, option(string)) /* [a-z] */
    | Empty /* epsilon */;
};

module DSL = {
  open Parsing;
  let star = x => Star(x);
  let plus = x => Plus(x);
  let maybe = x => Optional(x);
  let group = x => Group(x);
  let t = (~label=?, t) => Terminal(t, label);
  let n = (~label=?, t) => NonTerminal(t, label);
  let chars = (a, b) => Chars(a, b, None);
  let hugLeft = x => NoSpaceBefore(x);
  let hugRight = x => NoSpaceAfter(x);
};



let unwrapOr = (a, b) =>
  switch a {
  | Some(x) => x
  | None => b
  };

module Path = {
  [@deriving show]
  type pathItem =
    | Item(Parsing.parsing, int)
    | Iter(int)
    | Choice(int, string);
};

module Error = {
  [@deriving show]
  type errors = (int, list((bool, list(Path.pathItem))));
  [@deriving show]
  type partial = (int, errors);
  let errorText = ((isNot, rule)) =>
    switch rule {
    | Parsing.Terminal(text, label) => "Expected \"" ++ (String.escaped(text) ++ "\"")
    | Parsing.Chars(start, cend, label) => Printf.sprintf("Expected %c..%c", start, cend)
    | Parsing.NonTerminal(name, label) => name
    | Parsing.Any(label) => "Any"
    | Parsing.Star(_) => "Star"
    | Parsing.Plus(_) => "Plus"
    | Parsing.Optional(_) => "Optional"
    | Parsing.EOF => "End of Input"
    | Parsing.CommentEOL => "Expected a newline (with optional comments)"
    | _ => "Unknown problem"
    };
  let errorPathItemText = (isNot, pathItem) =>
    switch pathItem {
    | Path.Choice(n, name) => string_of_int(n) ++ (":" ++ name)
    | Path.Item(item, loopIndex) => string_of_int(loopIndex) ++ (":" ++ errorText((isNot, item))) /*^ " [" ^ (string_of_int loopIndex) ^ "]"*/
    | Path.Iter(n) => "*" ++ string_of_int(n)
    };
  let rec errorPathText = (isNot, path, collect) =>
    switch path {
    | [] => collect
    | [pathItem, ...path] =>
      errorPathText(isNot, path, [errorPathItemText(isNot, pathItem), ...collect])
    };
  let lastLineLength = (txt, pos) =>
    if (pos >= String.length(txt)) {
      String.length(txt)
    } else {
      try {
        let atNewline = txt.[pos] == '\n';
        let mpos = atNewline ? pos - 1 : pos;
        let lastPos = String.rindex_from(txt, mpos, '\n');
        pos - lastPos - 1
      } {
      | Not_found => pos
      }
    };
  let leftPad = (base, num, coll) => {
    let res = ref("");
    for (i in 0 to num) {
      res := base ++ res^
    };
    res^
  };
  let rec slice = (lst, start) =>
    switch (start, lst) {
    | (0, _) => lst
    | (_, [_, ...rest]) => slice(rest, start - 1)
    | _ => raise(Failure("Invalid slice"))
    };
  let errorsText = (errors) =>
    String.concat(
      "",
      List.map(
        ((isNot, errPath)) => {
          let parts = errorPathText(isNot, errPath, []);
          let count = List.length(parts);
          let parts =
            if (count < 5) {
              parts
            } else {
              ["...", ...slice(parts, count - 5)]
            };
          Printf.sprintf("%s\n", String.concat(" > ", parts))
        },
        errors
      )
    );
  let genErrorText = (text, (pos, errors)) => {
    let (showText, pad) =
      if (pos <= 0) {
        (text, 0)
      } else {
        (
          String.sub(
            text,
            0,
            try (String.index_from(text, pos, '\n')) {
            | Not_found => String.length(text)
            }
          ),
          lastLineLength(text, pos) - 1
        )
      };
    Printf.sprintf("%s\n%s^\n", showText, leftPad("-", pad, "")) ++ errorsText(errors)
  };
};

exception ConversionError(Location.t, string, string)

module Result = {
  /* type resultType =
     | Terminal string
     | Lexical (string, string, int) string bool
     | Nonlexical (string, string, int) bool [@@deriving (yojson, show)]; */
  [@deriving (yojson, show)]
  type rule = (string, string);
  [@deriving (yojson, show)]
  type loc = Location.t;
  [@deriving (yojson, show)]
  type result =
    | Leaf(rule, string, loc)
    | Node(rule, list((string, result)), loc) /* label, child */;

  let white = n => {
    let buffer = Buffer.create(n);
    for (_ in 0 to n) {
      Buffer.add_char(buffer, ' ');
    };
    Buffer.contents(buffer)
  };
  let showPos = ({Lexing.pos_lnum, pos_cnum, pos_bol}) => {
    Printf.sprintf("%d:%d(%d)", pos_lnum, pos_cnum - pos_bol, pos_cnum)
  };
  let showLoc = ({Location.loc_start, loc_end}) => {
    showPos(loc_start) ++ " - " ++ showPos(loc_end)
  };
  let rec showNode = (label, node, indent) => switch node {
    | Leaf((rule, sub), string, loc) => (label == "" ? "" : "[" ++ label ++ "]") ++ rule ++ "(" ++ (sub == "" ? "" : sub ++ ", ") ++ showLoc(loc) ++ ")" ++ ": " ++ String.escaped(string)
    | Node((rule, sub), children, loc) =>
      (label == "" ? "" : "[" ++ label ++ "]") ++ rule ++ "(" ++ (sub == "" ? "" : sub ++ ", ") ++ showLoc(loc) ++ ")"
      ++ String.concat(
        "\n",
        List.map(((label, node)) => white(indent) ++ showNode(label, node, indent + 2), children)
      )
  };
  type parserMatch = Belt.Result.t(result, (option(result), Error.partial));
};
