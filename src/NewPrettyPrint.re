open PackTypes.Result;

open PackTypes.Parsing;


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

let findByLabel = (children, needle) => {
  switch (maybeFind(children, ((label, child)) => label == needle ? Some(child) : None)) {
    | (None, c) => (Belt.Result.Error("No child found for "), c)
    | (Some(c), u) => (Ok(c), u)
  }
};

let findByType = (children, needle) =>
  switch (maybeFind(
    children,
    ((label, child)) =>
      if (label == "") {
        switch child {
        | Leaf((name, sub), _, _) as child
        | Node((name, sub), _, _, _) as child when name == needle => Some(child)
        | _ => None
        }
      } else {
        None
      }
  )) {
    | (Some(x), c) => (Belt.Result.Ok(x), c)
    | (None, c) => (Error("Cannot find child for type " ++ needle), c)
  }

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


let break = Pretty.line("");
let space = Pretty.line(" ");
let dedent = Pretty.back(2, "");

let str = Pretty.text;
let (@!) = Pretty.append;

/* 
let sepd_list = (sep, items, loop) => {
  let rec recur = items => switch items {
    | [] => Pretty.empty
    | [one] => loop(one)
    | [one, ...more] => loop(one) @! sep @! recur(more)
  };
  recur(items)
};

let commad_list = (loop, items) => {
  sepd_list(str(",") @! space, items, loop)
}; */



open Belt.Result;

let combine = (item, res, sep) => {
  /* print_endline("Combining"); */

    switch (item, res) {
    | (one, `Empty) => one
    | (`Empty, one) => one

    | (`Normal(a), `Normal(b)) => `Normal(a @! sep @! b)
    | (`Normal(a), `Left(b)) => `Normal(a @! b)
    | (`Normal(a), `Right(b)) => `Right(a @! sep @! b)
    | (`Normal(a), `Both(b)) => `Right(a @! b)

    | (`Both(a), `Normal(b)) => `Left(a @! b)
    | (`Both(a), `Left(b)) => `Left(a @! b)
    | (`Both(a), `Right(b)) => `Both(a @! b)
    | (`Both(a), `Both(b)) => `Both(a @! b)

    | (`Left(a), `Normal(b)) => `Left(a @! sep @! b)
    | (`Left(a), `Left(b)) => `Left(a @! b)
    | (`Left(a), `Both(b)) => `Both(a @! b)
    | (`Left(a), `Right(b)) => `Both(a @! sep @! b)

    | (`Right(a), `Normal(b)) => `Normal(a @! b)
    | (`Right(a), `Left(b)) => `Normal(a @! b)
    | (`Right(a), `Right(b)) => `Right(a @! b)
    | (`Right(a), `Both(b)) => `Right(a @! b)
    };
};

let unwrap = item => switch item {
  | `Empty => Pretty.empty
  | `Normal(a) | `Left(a) | `Right(a) | `Both(a) => a
};

let map = (fn, item) => switch item {
  | `Empty => `Empty
  | `Normal(a) => `Normal(fn(a))
  | `Left(a) => `Left(fn(a))
  | `Right(a) => `Right(fn(a))
  | `Both(a) => `Both(fn(a))
};

let left = item => switch item {
  | `Empty => `Empty
  | `Normal(a) => `Left(a)
  | `Left(a) => `Left(a)
  | `Right(a) => `Both(a)
  | `Both(a) => `Both(a)
};

let right = item => switch item {
  | `Empty => `Empty
  | `Normal(a) => `Right(a)
  | `Right(a) => `Right(a)
  | `Left(a) => `Both(a)
  | `Both(a) => `Both(a)
};

let rec greedy = (isLexical, loop, p, children, min, max) =>
  /* Printf.eprintf "Greedy %d %d\n" min max; */
  if (max == 0) {
    Ok((`Empty, children))
  } else {
    switch (loop([p], children)) {
      | Error(message) => min <= 0 ? Ok((`Empty, children)) : Error(message)
      | Ok((res, unused)) when children == unused => Ok((res, unused))
      | Ok((res, unused)) =>
        switch (greedy(isLexical, loop, p, unused, min - 1, max - 1)) {
          | Ok((r2, u2)) when r2 == `Empty => Ok((res, u2))
          | Ok((r2, u2)) => Ok((combine(res, r2, isLexical ? break : space), u2))
          | Error(message) => min <= 1 ? Ok((res, unused)) : Error(message)
        }
    }
  };

let prependItem = (sep, item, k) => {
  let%try (res, unused) = k;
  Ok((combine(item, res, sep), unused));
};

/* let concatItem = (item, (success, res, unused)) => (success, item @ res, unused); */

/* let rec spacedDoc = items => switch items {
  | [] => Pretty.empty
  | [one] => one
  | [one, ...rest] => one @! space @! spacedDoc(rest)
}; */

/* let rec linedDoc = items => switch items {
  | [] => Pretty.empty
  | [one] => one
  | [one, ...rest] => Pretty.dontFlatten(one) @! linedDoc(rest)
}; */

let rec singleOutput = (grammar, ignoringNewlines, isLexical, item, children, loop) => {
  switch item {
  | Terminal(text, None) => Ok((`Normal(str(text)), children))
  | Terminal(text, Some(label)) =>
    switch (findByLabel(children, label)) {
    | (Error(m), _) => Error(m)
    | (Ok(x), children) =>
      Ok((`Normal(str(text)), children))
    }
  | NonTerminal(name, label) => processNonTerminal(grammar, name, label, children, ignoringNewlines, loop)
  | NoSpaceAfter(p) =>
    let%try (a, b) = singleOutput(grammar, ignoringNewlines, isLexical, p, children, loop);
    Ok((right(a), b))
  | NoSpaceBefore(p) =>
    let%try (a, b) = singleOutput(grammar, ignoringNewlines, isLexical, p, children, loop);
    Ok((left(a), b))
  | Lexify(p) => singleOutput(grammar, ignoringNewlines, isLexical, p, children, loop)
  | Group(p) => loop(ignoringNewlines, p, children);
  | CommentEOL => Ok((`Normal(Pretty.breakAfter("")), children))
  | EOF | Empty | Lookahead(_) | Not(_) => Ok((`Empty, children))

  | Star(p) => greedy(isLexical, loop(ignoringNewlines), p, children, 0, -1)
  | Plus(p) => greedy(isLexical, loop(ignoringNewlines), p, children, 1, -1)
  | Optional(p) => {
    let%try (res, unused) = greedy(isLexical, loop(ignoringNewlines), p, children, 0, 1);
    if (unused == children) {
      Ok((`Empty, unused))
    } else {
      Ok((res, unused))
    }
  }

  | Any(_) | Chars(_) => Error("Chars should be within a @leaf, not at the top level")
  }
}
and outputItem = (grammar, ~isLexical, ignoringNewlines, items, children) => {
  let loop = outputItem(grammar, ~isLexical);
  switch children {
    | [("", Comment(EOL, contents, _)), ...rest] => {
      loop(ignoringNewlines, items, rest) |> prependItem(break, `Normal(Pretty.breakAfter(contents)))
    }
    /* TODO check to see that it's multiline */
    | [("", Comment(Multi | Doc, contents, _)), ...rest] =>
      loop(ignoringNewlines, items, rest) |> prependItem(break, `Normal(Pretty.multiLine(contents)))
    | _ => switch items {
      | [] => Ok((`Empty, children))

      /* | [("", Indent), ...rest] =>
        let%try (res2, unused) = loop(ignoringNewlines, rest, unused);
        Ok((map(m => Pretty.indent(4, m), res2), unused)) */

      | [item] => singleOutput(grammar, ignoringNewlines, isLexical, item, children, loop)

      | [item, ...rest] =>
        /* Ok, right here I can do a >> indent thingy */
        let%try (res, unused) = singleOutput(grammar, ignoringNewlines, isLexical, item, children, loop);
        let%try (res2, unused) = loop(ignoringNewlines, rest, unused);
        if (res == `Empty) {
          Ok((res2, unused))
        } else if (isLexical) {
          Ok((combine(res, res2, break), unused))
        } else {
          Ok((combine(res, res2, space), unused))
        }
      }
    }
}

and processNonTerminal = (grammar, name, label, children, ignoringNewlines, loop) =>
  switch (passThroughChildren(grammar, name)) {
  | Some((subs, ignoreNewlines)) =>
    /* print_endline "passthrough"; */
    let newIgnore =
      switch (ignoreNewlines, ignoringNewlines) {
      | (Yes, _) => true
      | (No, _) => false
      | (Inherit, x) => x
      };
    let%try (res, unused) = loop(newIgnore, subs, children);
    /* let output =
      switch res {
      | [sub] => sub
      | res =>
        if (ignoreNewlines == Yes) {
          Output.MaybeNewlined(res)
        } else {
          makeStraightWithEOLs(res)
        }
      }; */
    Ok((res, unused))
  | None =>
    let (child, others) =
      switch label {
      | Some(label) => findByLabel(children, label)
      | None => findByType(children, name)
      };
    let%try result = child;
    /* print_endline("Nonterminal " ++ name ++ " with result " ++ PackTypes.Result.showNode("", result, 0)); */
    let%try output = resultToPretty(ignoringNewlines, grammar, result);
    Ok((`Normal(
      /* Pretty.group(output) */
      Pretty.group(Pretty.indent(4, output))
      /* Pretty.indent(4, Pretty.group(output)) */
    ), others));

        /* let (success, res, unused) = loop(ignoringNewlines, rest, others);
        let children =
          switch output {
          | Output.Newlined(x) => [Output.NoSpace, output, Output.NoSpace, ...res]
          | _ => [output, ...res]
          };
        (success, children, unused) */
  }



and resultToPretty: (bool, grammar, result) => Belt.Result.t(Pretty.doc, string) =
  (ignoringNewlines, grammar, result) =>
    switch result {
    | Leaf(_, contents, _) => Ok(Pretty.text(contents))
    | Comment(EOL, contents, _) => Ok(Pretty.breakAfter(contents))
    | Comment(Doc, contents, _) => Ok(Pretty.text(contents))
    | Comment(Multi, contents, _) => Ok(Pretty.text(contents))
    | Node((name, sub), children, _, _comments) =>
      /* print_endline("NOde to pretty " ++ name ++ " " ++ sub ++ " with children " ++ string_of_int(List.length(children))); */
      let%try res = nodeToPretty(ignoringNewlines, grammar, (name, sub), children);
      /* print_endline("Workded " ++ name); */
      Ok(res)
    }
and nodeToPretty = (ignoringNewlines, grammar, (ruleName, sub), children) => {
  let rule = List.assoc(ruleName, grammar.rules);
  let%try (_, _, items) = switch (List.find(((name, _, _)) => name == sub, rule.choices)) {
    | exception Not_found => Error("No rule sub " ++ ruleName ++ " : " ++ sub)
    | x => Ok(x)
  };
  let ignoringNewlines =
    switch (rule.ignoreNewlines, ignoringNewlines) {
    | (Yes, _) => true
    | (No, _) => false
    | (Inherit, x) => x
    };
  let isLexical = Char.uppercase(ruleName.[0]) != ruleName.[0];
  let%try (result, unused) = outputItem(grammar, ~isLexical, ignoringNewlines, items, children);

  switch unused {
    | [] => Ok(unwrap(result))
    | _ => {
      Error("Failed to print " ++ ruleName ++ " : " ++ sub)
    }
  }
};











let prettyString = (~width=100, doc) => {
  let buffer = Buffer.create(100);
  Pretty.print(~width, ~output=(text => Buffer.add_string(buffer, text)), ~indent=(num => {
    /* Buffer.add_string(buffer, "\n"); */
    for (i in 1 to num) { Buffer.add_char(buffer, ' ') }
  }), doc);
  Buffer.to_bytes(buffer) |> Bytes.to_string
};

let toPretty = (grammar: grammar, result) => {
  resultToPretty(false, grammar, result);
};

let startToString = (~maxWidth=50, grammar, (sub, children, loc, comments)) => {
  let node = Node(("Start", sub), children, loc, comments);
  let%try pretty = resultToPretty(false, grammar, node);
  Ok(prettyString(~width=maxWidth, pretty))
};
