open PackTypes.Result;

open PackTypes.Parsing;

module P = PackTypes.Parsing;

module Output = {

  /*** TODO how do I whitespace? newlined things can be indented, yes, but how about whitespace between things? **/

  /*** I'm imagining maybe a rule annotation that says "everything has a splace between them" or sth. And then maybe
       a pseudo-item that says "suppress space here" or "add space here" **/
  type outputT =
    | Text(string)
    | EOL
    | NoSpace
    | MaybeNewlined(list(outputT))
    | Newlined(list(outputT))
    | Lexical(list(outputT))
    | Straight(list(outputT));
    /* | Newlined (list outputT) TODO I do think I want this... */
    /* [@@deriving show] */
};

type config = {
  maxWidth: int,
  indentWidth: int,
  indentStr: string
};

let pad = (num, base) => {
  let txt = ref("");
  for (i in 0 to num) {
    txt := txt^ ++ base
  };
  txt^
};

type iterim =
  | Text(string)
  | NoSpace;

let rec outputToString = (config, indentLevel, output) =>
  switch output {
  | Output.EOL => ("\n" ++ pad(indentLevel - 1, config.indentStr), true) /* TODO need to account for current indent level too */
  | Output.NoSpace => failwith("unhandled nospace")
  /* ("", false) /* TODO make sure this isn't happening much... */ */
  /* failwith "NoSpace should be handled by the parent" */
  | Output.Text(str) => (str, false) /* TODO check for newlines? */
  /*** TODO multiline strings -- should I be aware of that? Also lexical things that span multiple lines.. for some reason */
  | Output.Lexical(items) => (
      String.concat("", List.map((x) => fst(outputToString(config, 0, x)), items)),
      false
    )
  | Output.Straight(items) =>
    let rec loop = (items) =>
      switch items {
      | [] => ("", false)
      | [Output.NoSpace, ...rest] => loop(rest)
      | [child] => outputToString(config, indentLevel, child)
      | [child, Output.NoSpace, ...rest] =>
        let (restext, multi) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel, child);
        (res ++ restext, multi || nmulti)
      | [child, ...rest] =>
        let (restext, multi) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel, child);
        (res ++ (" " ++ restext), multi || nmulti)
      };
    loop(items)
  | Output.MaybeNewlined(items) =>
    let rec loop = (items) =>
      switch items {
      | [] => ([], 0, 0)
      | [Output.NoSpace, ...rest] =>
        let (items, len, multis) = loop(rest);
        /* let (res, nmulti) = outputToString config indentLevel child; */
        ([NoSpace, ...items], len, multis)
      | [child, ...rest] =>
        let (items, len, multis) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel + 1, child);
        ([Text(res), ...items], len + String.length(res), nmulti ? multis + 1 : multis)
      };
    let (items, total, multis) = loop(items);
    if (multis > 1
        || total
        + config.indentWidth
        * indentLevel > config.maxWidth
        && List.length(items) > 1) {
      let padt = "\n" ++ pad(indentLevel + 0, config.indentStr);
      let rec loop = (items) =>
        switch items {
        | [] => ""
        | [Text(child)] => child
        | [Text(child), NoSpace, ...rest] => child ++ loop(rest)
        | [Text(child), ...rest] => child ++ (padt ++ loop(rest))
        | [NoSpace, ...rest] => loop(rest)
        };
      (padt ++ (loop(items) ++ ("\n" ++ pad(indentLevel - 1, config.indentStr))), true)
    } else {
      let rec loop = (items) =>
        switch items {
        | [] => ""
        | [Text(child)] => child
        | [Text(child), NoSpace, ...rest] => child ++ loop(rest)
        | [Text(child), ...rest] => child ++ (" " ++ loop(rest))
        | [NoSpace, ...rest] => loop(rest)
        };
      (loop(items), false)
    }
  | Output.Newlined(items) =>
    let padt = "\n" ++ pad(indentLevel + 0, config.indentStr);
    let rec loop = (items) =>
      switch items {
      | [] => ("", false)
      | [Output.NoSpace, ...rest] => loop(rest)
      | [child] => outputToString(config, indentLevel + 1, child)
      | [child, Output.NoSpace, ...rest] =>
        let (restext, multi) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel + 1, child);
        (res ++ restext, multi || nmulti)
      | [child, ...rest] =>
        let (restext, multi) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel + 1, child);
        (res ++ (padt ++ restext), multi || nmulti)
      };
    let (str, multi) = loop(items);
    (padt ++ (str ++ ("\n" ++ pad(indentLevel - 1, config.indentStr))), multi)
  };

/* Finds an item and returns the list without that item */
let rec maybeFind = (children, check) =>
  switch children {
  | [] => (None, [])
  | [child, ...rest] =>
    switch (check(child)) {
    | None =>
      let (res, rest) = maybeFind(rest, check);
      (res, [child, ...rest])
    | x => (x, rest)
    }
  };

let findByLabel = (children, needle) =>
  maybeFind(children, ((label, child)) => label == needle ? Some(child) : None);

let findByType = (children, needle) =>
  maybeFind(
    children,
    ((label, child)) =>
      if (label == "") {
        switch child {
        | Leaf((name, sub), _, _) as child
        | Node((name, sub), _, _) as child when name == needle => Some(child)
        | _ => None
        }
      } else {
        None
      }
  );

let rec greedy = (loop, p, children, min, max) =>
  /* Printf.printf "Greedy %d %d\n" min max; */
  if (max == 0) {
    (true, [], children)
  } else {
    let (success, res, unused) = loop([p], children);
    if (! success) {
      (
        /* print_endline ("First greed aborted, and " ^ (min <= 0 ? "yup" : "nop")); */
        min <= 0,
        [],
        children
      )
    } else if (children == unused) {
      (
        /* didn't consume anything, limit to one */
        true,
        res,
        unused
      )
    } else {
      let (s2, r2, u2) = greedy(loop, p, unused, min - 1, max - 1);
      /* print_endline ("Inner greed " ^ (s2 ? "yup" : "nop")); */
      if (s2) {
        (true, List.concat([res, r2]), u2)
      } else if (min <= 1) {
        (true, res, unused)
      } else {
        (false, [], children)
      }
    }
  };

let passThroughChildren = (grammar, name) => {
  let rule =
    try (List.assoc(name, grammar.rules)) {
    | Not_found => failwith("Undefined rule name: " ++ name)
    };
  if (rule.passThrough) {
    let (a, b, c) = List.hd(rule.choices); /*** TODO test multiple? */
    Some((c, rule.ignoreNewlines))
  } else {
    None
  }
};

let makeStraightWithEOLs = (res) => {
  let rec loop = (items) =>
    switch items {
    | [] => []
    | [Output.EOL, ...rest] => loop(rest)
    | [item, Output.EOL, ...rest] => [[item], ...loop(rest)]
    | [item, ...rest] =>
      switch (loop(rest)) {
      | [firsts, ...others] => [[item, ...firsts], ...others]
      | [] => [[item]]
      }
    };
  let res = loop(res);
  let result =
    switch res {
    | [items] => Output.Straight(items)
    | [] => Output.Straight([])
    | _ => Output.Newlined(List.map((x) => Output.Straight(x), res))
    };
  /* print_endline (Output.show_outputT result); */
  result
};

let rec resultToOutput: (bool, grammar, result) => option(Output.outputT) =
  (ignoringNewlines, grammar, result) =>
    switch result {
    | Leaf(_, contents, _) => Some(Output.Text(contents))
    | Node((name, sub), children, _) =>
      nodeToOutput(ignoringNewlines, grammar, (name, sub), children)
    }
and processNonTerminal = (grammar, name, label, children, ignoringNewlines, rest, loop) =>
  switch (passThroughChildren(grammar, name)) {
  | Some((subs, ignoreNewlines)) =>
    /* print_endline "passthrough"; */
    let newIgnore =
      switch (ignoreNewlines, ignoringNewlines) {
      | (Yes, _) => true
      | (No, _) => false
      | (Inherit, x) => x
      };
    let (success, res, unused) = loop(newIgnore, subs, children);
    if (! success) {
      (false, [], children)
    } else {
      let (s2, r2, u2) = loop(ignoringNewlines, rest, unused);
      let output =
        switch res {
        | [sub] => sub
        | res =>
          if (ignoreNewlines == Yes) {
            Output.MaybeNewlined(res)
          } else {
            makeStraightWithEOLs(res)
          }
        };
      let children =
        switch output {
        | Output.Newlined(x) => [Output.NoSpace, output, Output.NoSpace, ...r2]
        | _ => [output, ...r2]
        };
      (s2, children, u2)
    }
  | None =>
    let (child, others) =
      switch label {
      | Some(label) => findByLabel(children, label)
      | None => findByType(children, name)
      };
    switch child {
    | None => (false, [], children)
    | Some(result) =>
      switch (resultToOutput(ignoringNewlines, grammar, result)) {
      | None => (false, [], children)
      | Some(output) =>
        let (success, res, unused) = loop(ignoringNewlines, rest, others);
        let children =
          switch output {
          | Output.Newlined(x) => [Output.NoSpace, output, Output.NoSpace, ...res]
          | _ => [output, ...res]
          };
        (success, children, unused)
      }
    }
  }
and nodeToOutput = (ignoringNewlines, grammar, (name, sub), children) => {
  /* Printf.printf "Output: %s %s\n" name sub; */
  let rule = List.assoc(name, grammar.rules);
  let (_, _, items) =
    try (List.find(((name, _, _)) => name == sub, rule.choices)) {
    | Not_found => failwith("Unknown rule sub " ++ (name ++ (" :: " ++ sub)))
    };
  let ignoringNewlines =
    switch (rule.ignoreNewlines, ignoringNewlines) {
    | (Yes, _) => true
    | (No, _) => false
    | (Inherit, x) => x
    };
  let isLexical = Char.uppercase(name.[0]) != name.[0];
  /* print_endline ("Ignoring newlines: " ^ (ignoringNewlines ? "yep" :" nop")); */
  let rec loop = (ignoringNewlines, items, children) =>
    switch items {
    | [] => (true, [], children)
    | [item, ...rest] =>
      switch item {
      | Terminal(text, None) =>
        let (success, res, unused) = loop(ignoringNewlines, rest, children);
        (success, [Output.Text(text), ...res], unused)
      | Terminal(text, Some(label)) =>
        switch (findByLabel(children, label)) {
        | (None, _) => (false, [], children)
        | (Some(x), children) =>
          let (success, res, unused) = loop(ignoringNewlines, rest, children);
          (success, [Output.Text(text), ...res], unused)
        }
      | Any(_)
      | Chars(_) => failwith("Chars shouldn't be at the top level")
      | NonTerminal(name, label) =>
        processNonTerminal(grammar, name, label, children, ignoringNewlines, rest, loop)
      | Lexify(p) => loop(ignoringNewlines, [p, ...rest], children)
      | NoSpaceAfter(p) =>
        let (success, res, unused) = loop(ignoringNewlines, [p], children);
        if (success) {
          let (s2, r2, u2) = loop(ignoringNewlines, rest, unused);
          (s2, List.concat([res, [Output.NoSpace], r2]), u2)
        } else {
          (success, res, unused)
        }
      | NoSpaceBefore(p) =>
        let (success, res, unused) = loop(ignoringNewlines, [p], children);
        if (success) {
          let (s2, r2, u2) = loop(ignoringNewlines, rest, unused);
          (s2, List.concat([[Output.NoSpace], res, r2]), u2)
        } else {
          (success, res, unused)
        }
      | Group(p) =>
        let (success, res, unused) = loop(ignoringNewlines, p, children);
        if (success) {
          let (s2, r2, u2) = loop(ignoringNewlines, rest, unused);
          (s2, List.concat([res, r2]), u2)
        } else {
          (success, res, unused)
        }
      | CommentEOL /* TODO print comments back */ =>
        let (success, res, unused) = loop(ignoringNewlines, rest, children);
        (success, [Output.EOL, ...res], unused)
      | EOF
      | Empty
      | Lookahead(_)
      | Not(_) => loop(ignoringNewlines, rest, children)
      | Star(p) =>
        let (success, res, unused) = greedy(loop(ignoringNewlines), p, children, 0, (-1));
        if (success) {
          let (s2, r2, u2) = loop(ignoringNewlines, rest, unused);
          (s2, List.concat([res, r2]), u2)
        } else {
          (success, res, unused)
        }
      | Plus(p) =>
        let (success, res, unused) = greedy(loop(ignoringNewlines), p, children, 1, (-1));
        if (success) {
          let (s2, r2, u2) = loop(ignoringNewlines, rest, unused);
          (s2, List.concat([res, r2]), u2)
        } else {
          (success, res, unused)
        }
      | Optional(p) =>
        let (success, res, unused) = greedy(loop(ignoringNewlines), p, children, 0, 1);
        if (success) {
          let (s2, r2, u2) = loop(ignoringNewlines, rest, unused);
          /* If we didn't consume anything, then don't produce anything -- this is for
               getting rid of empty syntax
             */
          if (unused == children) {
            (s2, r2, u2)
          } else {
            (s2, List.concat([res, r2]), u2)
          }
        } else {
          (success, res, unused)
        }
      }
    };
  let (success, res, unused) = loop(ignoringNewlines, items, children);
  switch unused {
  | [] =>
    Some(
      switch res {
      | [Output.EOL] => Output.NoSpace /* suppress orphan EOLs */
      | [sub] => sub
      | _ =>
        if (isLexical) {
          Output.Lexical(res)
        } else if (rule.ignoreNewlines == Yes) {
          Output.MaybeNewlined(res)
        } else {
          makeStraightWithEOLs(res)
        }
      }
    )
  | _ =>
    Printf.eprintf("Failed to print %s : %s\n", name, sub);
    /* print_endline "Some unused"; */
    None
  }
};

let toString = (~maxWidth=50, grammar: grammar, result) =>
  switch (resultToOutput(false, grammar, result)) {
  | Some(output) =>
    /* print_endline (Output.show_outputT output); */
    Some(
      String.trim(fst(outputToString({indentWidth: 2, indentStr: "  ", maxWidth}, (-1), output)))
    )
  | None => None
  };

let startToString = (~maxWidth=50, grammar, (sub, children, loc)) => {
  let node = Node(("Start", sub), children, loc);
  toString(~maxWidth, grammar, node)
};