
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
    capturesComments: bool,
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
    | Lookahead(parsing) /* &e */
    | Not(parsing) /* !e */
    | Lexify(parsing) /* # somelexrule */

    | Indent /* Indents by 2 */
    | FullIndent /* Indents to the current offset */
    | NoSpaceAfter(parsing) /* a printing rule, suppresses space */
    | NoSpaceBefore(parsing) /* a printing rule, suppresses space */


    | Any(option(string)) /* any */

    | EOF /* EOF */
    | CommentEOL
    | Group(list(parsing)) /* ( e ... ) */
    | NonTerminal(string, option(string)) /* nonterminal 'name' */
    | Terminal(string, option(string)) /* terminal */
    | Chars(char, char, option(string)) /* [a-z] */
    | Empty /* epsilon */;

  let showIgnore = i => switch i {
    | Yes => "Yes"
    | No => "No"
    | Inherit => "Inherit"
  };

  let showOption = (o, v) => switch o {
    | None => "None"
    | Some(x) => "Some(" ++ v(x) ++ ")"
  };

  let showString = s => Printf.sprintf("%S", s);

  let showChar = c => {
    "'" ++ Char.escaped(c) ++ "'"
  };

  let showBool = t => t ? "true" : "false";

  let showList = (~oneline=true, items, show) => oneline
  ? "[" ++ String.concat(", ", List.map(show, items)) ++ "]"
  : "[\n" ++ String.concat(",\n", List.map(show, items)) ++ "\n]";

  let rec showParsing = parsing => switch parsing {
    | Any(inner) => "Any(" ++ showOption(inner, showString) ++ ")"
    | Star(p) => "Star(" ++ showParsing(p) ++ ")"
    | Plus(p) => "Plus(" ++ showParsing(p) ++ ")"
    | Optional(p) => "Optional(" ++ showParsing(p) ++ ")"
    | NoSpaceAfter(p) => "NoSpaceAfter(" ++ showParsing(p) ++ ")"
    | NoSpaceBefore(p) => "NoSpaceBefore(" ++ showParsing(p) ++ ")"
    | Lookahead(p) => "Lookahead(" ++ showParsing(p) ++ ")"
    | Not(p) => "Not(" ++ showParsing(p) ++ ")"
    | Lexify(p) => "Lexify(" ++ showParsing(p) ++ ")"
    | EOF => "EOF"
    | Indent => "Indent"
    | FullIndent => "FullIndent"
    | CommentEOL => "CommentEOL"
    | Group(inner) => "Group(" ++ showList(inner, showParsing) ++ ")"
    | NonTerminal(name, label) => "NonTerminal(" ++ showString(name) ++ ", " ++ showOption(label, showString) ++ ")"
    | Terminal(name, label) => "Terminal(" ++ showString(name) ++ ", " ++ showOption(label, showString) ++ ")"
    | Empty => "Empty"
    | Chars(startC, endC, label) => "Chars(" ++ showChar(startC) ++ ", " ++ showChar(endC) ++ ", " ++ showOption(label, showString) ++ ")"
  };

  let showChoice = ((name, comment, parsings)) => "(" ++ showString(name) ++ ", " ++ showString(comment) ++ ", " ++ showList(parsings, showParsing) ++ ")";

  let showRule = ({passThrough, ignoreNewlines, leaf, docs, choices}) => Printf.sprintf({|{
    passThrough: %s,
    ignoreNewlines: %s,
    capturesComments: false,
    leaf: %s,
    docs: %s,
    choices: %s,
  }|}, showBool(passThrough), showIgnore(ignoreNewlines), showBool(leaf), showOption(docs, showString), showList(~oneline=List.length(choices) == 1, choices, showChoice));

  let showGrammar = ({lineComment, blockComment, rules}) => Printf.sprintf(
    "{lineComment: %s, blockComment: %s, rules: %s}",
    showOption(lineComment, showString),
    showOption(blockComment, ((a, b)) => "(" ++ showString(a) ++ ", " ++ showString(b) ++ ")"),
    showList(rules, ((name, rule)) => "(" ++ showString(name) ++ ", " ++ showRule(rule) ++ ")")
  );
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
      if (pos.Lexing.pos_cnum <= 0) {
        (text, 0)
      } else {
        (
          String.sub(
            text,
            0,
            try (String.index_from(text, pos.pos_cnum, '\n')) {
            | Not_found => String.length(text)
            }
          ),
          lastLineLength(text, pos.pos_cnum) - 1
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

  type loc = Location.t;
  type comment = (string, loc);
  type comments = (option(comment), list(comment), list(comment), option(comment));

  type rule = (string, string);
  type commentType = Doc | Multi | EOL;
  type result =
    | Leaf(rule, string, loc)
    | Comment(commentType, string, loc)
    | Node(rule, list((string, result)), loc, option(comments)) /* label, child */;

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
    | Comment(t, string, loc) => "Comment: " ++ string
    | Node((rule, sub), children, loc, comments) =>
      (label == "" ? "" : "[" ++ label ++ "]") ++ rule ++ "(" ++ (sub == "" ? "" : sub ++ ", ") ++ showLoc(loc) ++ ")"
      ++ String.concat(
        "\n",
        List.map(((label, node)) => white(indent) ++ showNode(label, node, indent + 2), children)
      )
  };
  type parserMatch = Belt.Result.t(result, (option(result), Error.partial));
};
