module P = PackTypes.Parsing;

module R = PackTypes.Result;

let memo: Hashtbl.t(string, string) = Hashtbl.create(100);

let mLoc = Location.none;

let optOr = (orr, opt) =>
  switch opt {
  | None => orr
  | Some(x) => x
  };

let maybePrint = (grammar, result) =>
  try (PrettyPrint.toString(grammar, result)) {
  | Failure(message) => Some(message)
  };



let rec generateForItem = (grammar, table, depth, item) =>
  switch item {
  | P.NonTerminal(name, label) => [
      (optOr("", label), generateForRule(grammar, table, name, depth + 1))
    ]
  /* [((optOr "" label), R.Leaf (name, "") ("<" ^ name ^ ">") mLoc)] */
  | P.Terminal(contents, label) =>
    switch label {
    | Some(label) => [(label, R.Leaf(("", ""), contents, mLoc))]
    | None => []
    }
  | P.Lexify(p)
  | P.NoSpaceAfter(p)
  | P.NoSpaceBefore(p)
  | P.Star(p)
  | P.Plus(p)
  | P.Optional(p) => generateForItem(grammar, table, depth, p)
  | P.Group(p) => List.concat(List.map(generateForItem(grammar, table, depth), p))
  | P.Not(_)
  | P.Any(_)
  | P.Lookahead(_)
  | P.EOF
  | P.Empty
  | P.CommentEOL => []
  | P.Chars(start, cend, label) =>
    let s = Char.code(start);
    [
      (
        optOr("", label),
        R.Leaf(("", ""), Printf.sprintf("%c", Char.chr(Random.int(Char.code(cend) - s) + s)), mLoc)
      )
    ]
  }
and generateForRule = (grammar, table, rulename, depth) =>
  try (R.Leaf((rulename, ""), Hashtbl.find(table, rulename), mLoc)) {
  | Not_found =>
    if (depth > 3) {
      R.Leaf((rulename, ""), "<" ++ (rulename ++ ">"), mLoc)
    } else {
      let rule = List.assoc(rulename, grammar.P.rules);
      let choice = Random.int(List.length(rule.P.choices));
      let (sub, _, items) = List.nth(rule.P.choices, choice);
      R.Node(
        (rulename, sub),
        List.concat(List.map(generateForItem(grammar, table, depth), items)),
        mLoc,
        None
      )
    }
  };

let generateForChoice = (grammar, table, rule, items) =>
  R.Node(rule, List.concat(List.map(generateForItem(grammar, table, 5), items)), mLoc, None);

let generateExamples = (grammar, ruleName, table) => {
  let {P.choices, _} = List.assoc(ruleName, grammar.P.rules);
  List.map(
    ((sub, comment, items)) =>
      sub
      ++ (
        ":\n"
        ++ (
          generateForChoice(grammar, table, (ruleName, sub), items)
          |> maybePrint(grammar)
          |> optOr("Got nothing while printing")
        )
      ),
    choices
  )
  |> String.concat("\n")
};
/* let generateExamples grammar => {
     (
       List.map
       (fun (name, rule) => {
         List.map
         (fun (sub, comment, items) => {
           name ^ " - " ^ sub ^ ":  " ^ (generateForRule (name, sub) items |> maybePrint grammar |> optOr "Unable to pretty print")
         })
         rule.P.choices
       })
       grammar.P.rules
     ) |> List.concat |> String.concat "\n"
   }; */

let isLexical = ruleName => {let l = String.sub(ruleName, 0, 1); l != String.capitalize(l)};

let rec simpleForItem = (grammar, item) =>
  switch item {
  | P.NonTerminal(name, label) => simpleForRule(grammar, name)
  | P.Terminal(contents, label) => [`Text(
    {
      let c = contents
    |> Str.global_replace(
      Str.regexp_string("|"),
      "\\|"
    )
    |> Str.global_replace(
      Str.regexp_string("<"),
      "&lt;"
    )
    |> Str.global_replace(
      Str.regexp_string(">"),
      "&gt;"
    );
    if (c == "\\") { "\\\\" } else { c }
    }
  )]
  | P.NoSpaceAfter(p) => simpleForItem(grammar, p) @ [`Collapse]
  | P.NoSpaceBefore(p) => [`Collapse, ...simpleForItem(grammar, p)]

  | P.Lexify(p)

  | P.Star(p) => simpleForItem(grammar, p) @ [`Collapse, `Text("<sup>*</sup>")]
  | P.Plus(p) => simpleForItem(grammar, p) @ [`Collapse, `Text("<sup>+</sup>")]
  | P.Optional(p) => simpleForItem(grammar, p) @ [`Collapse, `Text("<sup>?</sup>")]

  | P.Group(p) => [`Text("⦅"), `Collapse, ...List.concat(List.map(simpleForItem(grammar), p))] @ [`Collapse, `Text("⦆")]

  | P.Any(_) => [`Text("<i>any</i>")]
  | P.Not(_)
  | P.Lookahead(_)
  | P.EOF
  | P.Empty
  | P.CommentEOL => []
  | P.Chars(start, cend, label) => [`Text(Char.escaped(start) ++ "…" ++ Char.escaped(cend))]
  }
and simpleForRule = (grammar, rulename) => {
  let rule = List.assoc(rulename, grammar.P.rules);
  let (sub, comment, items) = List.hd(rule.P.choices);
  if (List.length(rule.P.choices) > 1 || comment != "" || rule.leaf) {
    [`Text("<a href=\"#" ++ String.lowercase(rulename) ++ "\">" ++ rulename ++ "</a>")]
  } else {
    let res = List.concat(List.map(simpleForItem(grammar), items));
    if (isLexical(rulename)) {
      [`Text(String.concat("", List.map(m => switch m { | `Text(a) => a | `Collapse => ""}, res)))]
    } else {
      res
    }
  }
};

let help = {|
This grammar is displayed using a syntax similar to regular expressions.

- `⦅` and `⦆` "thick parenthesis" indicate grouping, to distinguish from parenthesis that are actually part of the grammar
- <code><sup>+</sup></code> allows one or more of the preceeding term or group
- <code><sup>*</sup></code> allows zero or more of the preceeding term or group
- <code><sup>?</sup></code> allows zero or one of the preceeding term or group

Note that this sacrifices a bit of precision in the interest of readability. For the source of truth, read the source code :D
|};

let simpleForChoice = (grammar, items) => List.concat(List.map(simpleForItem(grammar), items));

let rec showSimple = (items) => switch items {
  | [`Text(a), `Text(b), ...rest] => a ++ " " ++ showSimple([`Text(b), ...rest])
  | [`Text(a), `Collapse, ...rest] => a ++ showSimple(rest)
  | [`Text(a)] => a
  | [`Collapse, ...rest] => showSimple(rest)
  | [] => ""
};

let showSimple = (items, ruleName) => {
  let isLexical = {let l = String.sub(ruleName, 0, 1); l != String.capitalize(l)};
  if (isLexical) {
    String.concat("", List.map(m => switch m { | `Text(a) => a | `Collapse => ""}, items))
  } else {
    showSimple(items)
  }
};

let docsForGrammar = grammar => {
  open PackTypes.Parsing;
  List.map(((name, rule)) => {
    let (sub, comment, items) = List.hd(rule.choices);
    if (List.length(rule.choices) > 1) {
      Printf.sprintf("### %s\n\n", name) ++
      (switch rule.docs {
        | None => ""
        | Some(docs) => docs ++ "\n\n"
      }) ++ "| Name | Syntax |\n| --- | --- |\n" ++
      (rule.choices |> List.map(((sub, comment, items)) => {
        "| <i>" ++ sub ++ "</i> | " ++ "<code>" ++ showSimple(simpleForChoice(grammar, items), name) ++ "</code> |"
      }) |> String.concat("\n")) ++ "\n\n"
    } else if (comment != "") {
      "### " ++ name ++ "\n\n" ++ comment ++ "\n\n" ++
      "<code>" ++ showSimple(simpleForChoice(grammar, items), name) ++ "</code>\n\n"
    } else {""}
  }, grammar.rules |> List.rev) |> String.concat("");
};