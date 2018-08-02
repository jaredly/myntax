open PackTypes.Result;

module P = PackTypes.Parsing;

module RU = ResultUtils;

type decoratorArg =
  | Bool(bool)
  | String(string)
  | Number(int);

let parseString = (contents) =>
  String.sub(contents, 1, String.length(contents) - 2) |> Scanf.unescaped;

let parseDecorator = (children) => {
  let name = RU.getContentsByLabel(children, "name") |> RU.unwrap;
  let args =
    RU.getChildren(
      children,
      (child) =>
        switch child {
        | ("args", Node(("decarg", sub), children, _)) =>
          Some(
            switch sub {
            | "bool" =>
              Bool(RU.unwrap(RU.getContentsByType(children, "bool")) == "true" ? true : false)
            | "string" => String(parseString(RU.unwrap(RU.getContentsByType(children, "string"))))
            | "number" =>
              Number(int_of_string(RU.unwrap(RU.getContentsByType(children, "number"))))
            | _ => failwith("unexpected arg type")
            }
          )
        | _ => None
        }
    );
  (name, args)
};

let optOr = (orr, opt) =>
  switch opt {
  | Some(x) => x
  | None => orr
  };

let getFlag = (children) =>
  RU.getChild(
    children,
    (child) =>
      switch child {
      | ("flag", Node((_, sub), _, _)) => Some(sub)
      | ("flag", _) => failwith("Flag expected to be non-leaf")
      | _ => None
      }
  );

let unescapeString = (txt) =>
  try (String.length(txt) == 1 ? txt : Scanf.unescaped(txt)) {
  | Scanf.Scan_failure(message) => failwith("Unescape fail --" ++ (txt ++ "--"))
  };

let unescapeChar = (txt) => (String.length(txt) == 1 ? txt : unescapeString(txt)).[0];

let isSome = (x) =>
  switch x {
  | Some(_) => true
  | None => false
  };

let getSuffix = (children) =>
  RU.getChild(
    children,
    (child) =>
      switch child {
      | ("suffix", Node((_, sub), _, _)) => Some(sub)
      | ("suffix", _) => failwith("Suffix expected to be non-leaf")
      | _ => None
      }
  );

let unwrapString = (txt) => unescapeString(String.sub(txt, 1, String.length(txt) - 2));

let rec parseInner = (label, ((_, sub), children, loc)) =>
  if (sub == "nested") {
    if (isSome(label)) {
      failwith("groups can't have labels: " ++ RU.unwrap(label))
    };
    P.Group(
      RU.getChildren(
        children,
        ((label, child)) =>
          if (label == "nested") {
            switch child {
            | Node(_, children, _) => Some(parseItem(children))
            | _ => failwith("Nested child expected to be non-leaf")
            }
          } else {
            /* Printf.eprintf "umm %s %s\n" label (PackTypes.Result.show_result child); */
            None
          }
      )
    )
  } else {
    RU.getChild(
      children,
      ((_, child)) =>
        switch child {
        | Leaf(("string", _), contents, _) => Some(P.Terminal(unwrapString(contents), label))
        | Leaf(("ident", _), "any", _) => Some(P.Any(label))
        | Leaf(("ident", _), "EOF", _) => Some(P.EOF)
        | Leaf(("ident", _), "EOL", _) => Some(P.CommentEOL)
        | Leaf(("ident", _), contents, _) => Some(P.NonTerminal(contents, label))
        | Node(("char", _), children, _) =>
          RU.getChild(
            children,
            ((_, child)) =>
              switch child {
              | Leaf(("single", _), contents, _) =>
                Some(P.Terminal(unescapeString(contents), label))
              | _ => None
              }
          )
        | Node(("char_range", _), children, _) =>
          let start = RU.getContentsByLabel(children, "start") |> RU.unwrap;
          let send = RU.getContentsByLabel(children, "end") |> RU.unwrap;
          Some(P.Chars(unescapeChar(start), unescapeChar(send), label))
        | _ => None
        }
    )
    |> RU.unwrap
  }
and parseItem = (children) => {
  let neg = RU.getPresenceByLabel(children, "neg");
  let lexify = RU.getPresenceByLabel(children, "lexify");
  let noSpaceAfter = RU.getPresenceByLabel(children, "noSpaceAfter");
  let noSpaceBefore = RU.getPresenceByLabel(children, "noSpaceBefore");
  let suffix = getSuffix(children);
  let _ = getFlag(children); /* TODO use flags? */
  let label = RU.getContentsByLabel(children, "name");
  let inner = RU.getNodeByLabel(children, "inner") |> RU.unwrap |> parseInner(label);
  let inner = noSpaceAfter ? P.NoSpaceAfter(inner) : inner;
  let inner = noSpaceBefore ? P.NoSpaceBefore(inner) : inner;
  let inner =
    switch suffix {
    | None => inner
    | Some("plus") => P.Plus(inner)
    | Some("star") => P.Star(inner)
    | Some("opt") => P.Optional(inner)
    | _ => failwith("unexpected suffix")
    };
  let inner = neg ? P.Not(inner) : inner;
  let inner = lexify ? P.Lexify(inner) : inner;
  inner
};

let parseChoice = (children) => {
  let name = RU.getContentsByLabel(children, "name") |> optOr("");
  let comment = RU.getContentsByLabel(children, "comment") |> optOr("");
  let children =
    RU.getChildren(
      children,
      ((label, child)) =>
        switch child {
        | Node(("Item", _), children, _) => Some(parseItem(children))
        /* | Leaf ("noSpace", _) _ _ => Some (P.NoSpace) */
        | _ => None
        }
    );
  (name, comment, children)
};

let parseRule = (children) => {
  let name = RU.getContentsByLabel(children, "name") |> RU.unwrap;
  let (newLines, passThrough, leaf) =
    List.fold_left(
      (flags, child) =>
        switch child {
        | ("decorators", Node(_, children, _)) =>
          let (white, pass, leaf) = flags;
          switch (parseDecorator(children)) {
          | ("ignoreNewlines", [Bool(whether)]) => (whether ? P.Yes : P.No, pass, leaf)
          | ("ignoreNewlines", []) => (P.Yes, pass, leaf)
          | ("passThrough", []) => (white, true, leaf)
          | ("leaf", []) => (white, pass, true)
          | ("lineComment", _)
          | ("blockComment", _) => flags
          | (name, _) =>
            Printf.eprintf("Ignoring decorator %s\n", name);
            flags
          }
        | _ => flags
        },
      (P.Inherit, false, false),
      children
    );
  (
    name,
    {
      P.passThrough,
      P.docs: None,
      P.ignoreNewlines: newLines,
      P.leaf,
      P.choices:
        RU.getChildren(
          children,
          ((_, child)) =>
            switch child {
            | Node(("Choice", _), children, _) => Some(parseChoice(children))
            | _ => None
            }
        )
    }
  )
};

let getToplevelDecorators = (children) =>
  List.fold_left(
    (decs, child) =>
      switch child {
      | ("decorators", Node(_, children, _)) =>
        let (line, block) = decs;
        switch (parseDecorator(children)) {
        | ("lineComment", [String(line)]) => (Some(line), block)
        | ("blockComment", [String(one), String(two)]) => (line, Some((one, two)))
        | _ => decs
        }
      | _ => decs
      },
    (None, None),
    children
  );

let convert = (result: result) =>
  switch result {
  | Node(("Start", _), children, _) =>
    let rules =
      RU.getChildren(
        children,
        ((_, child)) =>
          switch child {
          | Node(("Rule", _), children, _) => Some(parseRule(children))
          | _ => None
          }
      );
    let (lineComment, blockComment) =
      RU.getChild(
        children,
        ((label, child)) =>
          switch child {
          | Node(("Rule", _), children, _) => Some(getToplevelDecorators(children))
          | _ => None
          }
      )
      |> optOr((None, None));
    {P.lineComment, blockComment, rules}
  | _ => failwith("Base must be of type `start`")
  };
