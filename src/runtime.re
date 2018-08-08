
/* from https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/ with modifications */
module P = PackTypes.Parsing;

module R = PackTypes.Result;

module RP = PackTypes.Path;

/** STATEFUL STUFF
 * It would probably nice to thread this through,
 * but this is way easier at the moment :shrug: */;
/* let currentDocComment = ref(None);
let pendingComments = ref([]); */

let locForOffs = (a, b) => {
  Location.loc_start: a,
  loc_end: b,
  loc_ghost: false,
};

let emptyResult = (pos, name, isLexical) => (R.Leaf((name, ""), "", locForOffs(pos, pos)), false);

let mergeErrors = ((i1, errs1), (i2, errs2)) =>
  if (i1.Lexing.pos_cnum == i2.Lexing.pos_cnum) {
    (i1, List.concat([errs1, errs2]))
  } else if (i1.pos_cnum < i2.pos_cnum) {
    (
      i2,
      errs2
      /* (i2, List.concat [errs1, errs2]) */
    )
  } else {
    (
      i1,
      errs1
      /* (i1, List.concat [errs1, errs2]) */
    )
  };

/* exception Found(ans); */











let optOr = (orr, opt) =>
  switch opt {
  | Some(x) => x
  | None => orr
  };

open PackCore.T;

open RuntimeComments;

let rec loop = (
  ~isLexical,
  ~state,
  ~ignoringNewlines,
  ~grammar,
  ~parse,
  i, items, path, loopIndex, isNegated
) => {
  let loop = loop(~isLexical, ~state, ~ignoringNewlines, ~grammar, ~parse);
  /* If in a NonLexical context, skip whitespace before trying to match a rule
    * Unless there are no more items. */
  let (i, items, comments) =
    if (isLexical || items == []) {
      (i, items, [])
    } else {
      switch items {
      | [P.Lexify(p), ...rest] => (i, [p, ...rest], [])
      | _ =>
        let i = skipWhite(i, state.input, state.len, ignoringNewlines);
        let (i', comments) =
          switch (ignoringNewlines, grammar.P.blockComment, grammar.P.lineComment) {
          | (false, Some(x), _) => skipBlockComments(i, x, state.input, state.len, false)
          | (true, Some(x), None) => skipBlockComments(i, x, state.input, state.len, true)
          | (true, Some(x), Some(y)) =>
            skipBlockAndLineComments(i, x, y, state.input, state.len)
          | (true, None, Some(x)) => skipLineComments(i, x, state.input, state.len)
          | (false, None, Some(_))
          | (_, None, None) => (i, [])
          };
        /* Printf.printf "Skipped comments %d %d\n" i i'; */
        (i', items, comments)
      }
    };
  let (pos, results, errors) = switch items {
  | [P.Empty | P.Indent | P.FullIndent, ...rest] => loop(i, rest, path, loopIndex + 1, isNegated)
  | [P.NoSpaceAfter(p), ...rest]
  | [P.NoSpaceBefore(p), ...rest]
  | [P.NoBreakAfter(p), ...rest]
  | [P.NoBreakBefore(p), ...rest]
  | [P.Lexify(p), ...rest] => loop(i, [p, ...rest], path, loopIndex, isNegated)
  | [P.Lookahead(p), ...rest] =>
    let (i', _, err) = loop(i, [p], path, loopIndex + 1, isNegated);
    if (i' >= i) {
      loop(i, rest, path, loopIndex + 1, isNegated /* propagate errors */)
    } else {
      (Lexing.dummy_pos, [], err)
    }
  | [P.Group(g), ...rest] => loop(i, List.concat([g, rest]), path, loopIndex, isNegated)
  | [P.Not(p), ...rest] =>
    let (i', _, err) = loop(i, [p], [RP.Item(P.Not(p), loopIndex), ...path], 0, ! isNegated);
    if (i' >= i) {
      (Lexing.dummy_pos, [], err)
    } else {
      loop(i, rest, path, loopIndex + 1, isNegated /* propagate errors */)
    }
  | [P.CommentEOL as item, ...rest] =>
    switch grammar.P.lineComment {
    | None =>
      if (i.pos_cnum >= state.len || state.input.[i.pos_cnum] == '\n') {
        let i' = skipWhite(i, state.input, state.len, true);
        let (i'', children, rest_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
        /* TODO collect comments */
        (i'', children, rest_errs)
      } else {
        (Lexing.dummy_pos, [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
      }
    | Some(lineComment) =>
      /** TODO FIXME */
      let (i', comment) = skipLineComments(i, lineComment, state.input, state.len);
      let i' =
        if (i'.pos_cnum > i.pos_cnum || i'.pos_cnum >= state.len || state.input.[i.pos_cnum] != '\n') {
          i'
        } else
          {
            let i' = skipWhite(i', state.input, state.len, true);
            let (i', _comment) = skipLineComments(i', lineComment, state.input, state.len);
            i'
          };
          /* i' + 1 */
      if (i'.pos_cnum > i.pos_cnum || i'.pos_cnum >= state.len) {
        let (i'', children, rest_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
        /* TODO collect comments */
        (i'', children, rest_errs)
      } else {
        (
          /* Printf.printf "No actual skippage %d %d \"%s\"\n" i i' (String.sub state.input i 10); */
          Lexing.dummy_pos,
          [],
          (i, [(true, [RP.Item(item, loopIndex), ...path])])
        )
      }
    }
  | [P.NonTerminal(n, label) as item, ...rest] =>
    /* Printf.eprintf "[%s]> %s : %d\n" rulename n i; */
    let env = {PackCore.state, emptyResult, mergeErrors, emptyErrors: (Lexing.dummy_pos, [])};
    let (i', (result, passThrough), errs) =
      PackCore.apply_rule(
        ~env=env,
        ~parse=parse(grammar),
        n,
        i,
        ignoringNewlines,
        isNegated,
        [RP.Item(item, loopIndex), ...path]
      );
    /* Printf.eprintf "  <-- apply_rule %s %d :: %s %d -> %d\n" n (fst errs) rulename i i'; */
    /* Printf.eprintf "[%s]< %s : %d\n" rulename n i'; */
    /* Printf.eprintf "%d) %s\n" (fst errs) (PackTypes.Error.errorsText (snd errs)); */
    if (i' >= i) {
      let (i'', children, rest_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
      let children =
        passThrough ?
          switch result {
          | R.Node(_, subchildren, _, _comments) => List.concat([subchildren, children])
          | R.Comment(_) => failwith("Passthrough can't handle a comment")
          | R.Leaf(_) => failwith("Passthrough can't have a leaf node")
          } :
          [(label |> optOr(""), result), ...children];
      (i'', children, mergeErrors(errs, rest_errs))
    } else {
      (Lexing.dummy_pos, [], errs)
    }
  | [P.Terminal(target_string, label) as item, ...rest] =>
    let slen = String.length(target_string);
    if (i.pos_cnum + slen > state.len) {
      (Lexing.dummy_pos, [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
    } else {
      let sub = String.sub(state.input, i.pos_cnum, slen);
      if (sub == target_string) {
        let (i'', children, err) =
          loop({...i, pos_cnum: i.pos_cnum + slen}, rest, path, loopIndex + 1, isNegated); /* TODO line / col num */
        let children =
          switch label {
          | Some(x) => [
              (x, R.Leaf(("", target_string), target_string, locForOffs(i, {...i, pos_cnum: i.pos_cnum + slen}))),
              ...children
            ]
          | None => children
          };
        (i'', children, err)
      } else {
        (Lexing.dummy_pos, [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
      }
    }
  | [P.Any(label) as item, ...rest] =>
    if (i.pos_cnum >= state.len) {
      (Lexing.dummy_pos, [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
    } else {
      let (i'', children, err) = loop({...i, pos_cnum: i.pos_cnum + 1}, rest, path, loopIndex + 1, isNegated);
      let contents = String.sub(state.input, i.pos_cnum, 1);
      let children =
        switch label {
        | Some(x) => [(x, R.Leaf(("", contents), contents, locForOffs(i, {...i, pos_cnum: i.pos_cnum + 1}))), ...children]
        | None => children
        };
      (i'', children, err)
    }
  | [P.EOF, ...rest] =>
    if (i.pos_cnum >= state.len) {
      (
        i,
        [],
        (Lexing.dummy_pos, []) /* TODO should I have a leaf here? */
      )
    } else {
      (Lexing.dummy_pos, [], (i, [(true, [RP.Item(P.EOF, loopIndex), ...path])]))
    }
  | [P.Chars(c1, c2, label) as item, ...rest] =>
    if (i.pos_cnum >= state.len) {
      (Lexing.dummy_pos, [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
    } else if (state.input.[i.pos_cnum] >= c1 && state.input.[i.pos_cnum] <= c2) {
      let (i'', children, errs) = loop({...i, pos_cnum: i.pos_cnum + 1}, rest, path, loopIndex + 1, isNegated);
      let contents = String.sub(state.input, i.pos_cnum, 1);
      let children =
        switch label {
        | Some(x) => [(x, R.Leaf(("", contents), contents, locForOffs(i, {...i, pos_cnum: i.pos_cnum + 1}))), ...children]
        | None => children
        };
      (i'', children, errs)
    } else {
      (Lexing.dummy_pos, [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
    }
  | [P.Star(subr) as item, ...rest] =>
    let (i', subchildren, errs) =
      RuntimeUtils.greedy(~mergeErrors, ~emptyErrors=(Lexing.dummy_pos, []), loop, 0, None, subr, i, [RP.Item(item, loopIndex), ...path], 0, isNegated);
    if (i' >= i) {
      let (i'', children, more_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
      (i'', List.concat([subchildren, children]), mergeErrors(errs, more_errs))
    } else {
      (Lexing.dummy_pos, [], errs)
    }
  | [P.Plus(subr) as item, ...rest] =>
    let (i', subchildren, errs) =
      RuntimeUtils.greedy(~mergeErrors, ~emptyErrors=(Lexing.dummy_pos, []), loop, 1, None, subr, i, [RP.Item(item, loopIndex), ...path], 0, isNegated);
    if (i' >= i) {
      let (i'', children, more_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
      (i'', List.concat([subchildren, children]), mergeErrors(errs, more_errs))
    } else {
      (Lexing.dummy_pos, [], errs)
    }
  | [P.Optional(subr) as item, ...rest] =>
    let (i', subchildren, errs) =
      RuntimeUtils.greedy(~mergeErrors, ~emptyErrors=(Lexing.dummy_pos, []), loop, 0, Some(1), subr, i, [RP.Item(item, loopIndex), ...path], 0, isNegated);
    if (i' >= i) {
      let (i'', children, more_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
      (i'', List.concat([subchildren, children]), mergeErrors(errs, more_errs))
    } else {
      (Lexing.dummy_pos, [], errs)
    }
  | [] => (i, [], (Lexing.dummy_pos, []))
  };
  (pos, (comments |> List.map(m => ("", m))) @ results, errors)
};

let rec parse = (grammar, state, rulename, i, isLexical, ignoringNewlines, isNegated, path) => {
  /* Printf.eprintf ">> %s %d\n" rulename i; */
  let {P.ignoreNewlines, capturesComments, choices, passThrough, leaf} =
    try (List.assoc(rulename, grammar.P.rules)) {
    | Not_found =>
      Printf.eprintf("error in grammar: unknown rulename '%s'\n", rulename);
      exit(1)
    };
  let wasIgnoringNewlines = ignoringNewlines;
  let ignoringNewlines =
    switch (ignoreNewlines, ignoringNewlines) {
    | (P.Inherit, x) => x
    | (P.No, _) => false
    | (P.Yes, _) => true
    };
  let numChoices = List.length(choices);
  /* Try each choice in turn until one matches. */
  let rec process = (choices, prevErrors, choiceIndex) =>
    switch choices {
    | [] => (Lexing.dummy_pos, emptyResult(i, rulename, isLexical), prevErrors)
    | [(sub_name, comment, rs), ...otherChoices] =>
      
      let subPath = numChoices === 1 ? path : [RP.Choice(choiceIndex, sub_name), ...path];

      let (i', children, err) =
        loop(
          ~isLexical,
          ~state,
          ~ignoringNewlines,
          ~grammar,
          ~parse,
          i,
          rs,
          subPath,
          0,
          isNegated,
        );
      let errs = mergeErrors(prevErrors, err);
      /* Printf.eprintf "$$ %d [%d, %d] (%s - %d)\n" (fst errs) (fst prevErrors) (fst err) rulename choiceIndex; */
      /* Printf.eprintf "PARSE[%s:%d] %d) %s\n" rulename i (fst errs) (PackTypes.Error.errorsText (snd errs)); */
      if (i' >= i) {
        /* Printf.eprintf "<final>\n"; */
        let name = (rulename, sub_name);
        let loc = locForOffs(i, i');
        let result = (
          leaf ? R.Leaf(name, String.sub(state.input, i.pos_cnum, i'.pos_cnum - i.pos_cnum), loc) : {
            R.Node(name, children, loc, None)
          },
          passThrough
        );
        (i', result, errs)
      } else {
        process(otherChoices, errs, choiceIndex + 1)
      }
    };
  process(choices, (Lexing.dummy_pos, []), 0)
  /*** TODO if wasIgnoringNewlines == false && ignoringNewlines = true, then ignore any trailing newlines */
};

let parse = (~filename="no name", grammar: PackTypes.Parsing.grammar, start, input) => {
  /* startFile("File name"); */
  let pos = {
    Lexing.pos_fname: filename,
    pos_cnum: 0,
    pos_lnum: 1,
    pos_bol: 0,
  };
  let state = PackCore.initialState(input, pos);
  /* TODO ignoringNewlines should be configurable? */
  let (i, (result, _), errs) =
    PackCore.apply_rule(
      ~env={state, emptyResult, mergeErrors, emptyErrors: (Lexing.dummy_pos, [])},
      ~parse=parse(grammar),
      start,
      pos,
      false,
      false,
      [],
    );
  /* TODO add these in to the toplevel rule */
  let (i, trailingComments) = i.pos_cnum >=0 ? skipAllWhite(i, grammar, input, String.length(input)) : (i, []);
  if (i == Lexing.dummy_pos) {
    Belt.Result.Error((None, (0, fst(errs), errs)))
  } else if (i.pos_cnum < state.len) {
    Belt.Result.Error((Some(result), (i.pos_cnum, fst(errs), errs)))
  } else {
    Belt.Result.Ok(result)
  }
};
