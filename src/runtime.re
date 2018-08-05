
/* from https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/ with modifications */
module P = PackTypes.Parsing;

module R = PackTypes.Result;

module RP = PackTypes.Path;

/** STATEFUL STUFF
 * It would probably nice to thread this through,
 * but this is way easier at the moment :shrug: */;
let currentDocComment = ref(None);
let pendingComments = ref([]);

/* let pos = ref((0, 0)); */
let lno = ref(1);
let bols = ref([0]);
let lastBol = ref(0);
let fname = ref("- no file -");

let startFile = (name) => {
  fname := name;
  lno := 1;
  bols := [0];
  lastBol := 0;
};

let incLine = bol => {
  /* print_endline("Inc " ++ string_of_int(bol)); */
  if (bol > lastBol^) {
    bols := [bol, ...bols^];
    lastBol := bol;
    lno := lno^ + 1;
    /* print_endline("New line number " ++ string_of_int(lno^)); */
  }
};

let posForLoc = i => {
  let bols = bols^;
  let lno = lno^;
  let rec loop = (lno, bols) => switch bols {
    | [] => (0, 0)
    | [a, ...rest] => i >= a ? (lno, a) : loop(lno - 1, rest)
  };
  let (lno, bol) = loop(lno, bols);
  {
    Lexing.pos_cnum: i,
    pos_lnum: lno,
    pos_bol: bol,
    /* pos_bol: 0, */
    pos_fname: fname^
  }
};

let locForOffs = (a, b) => {
  Location.loc_start: posForLoc(a),
  loc_end: posForLoc(b),
  loc_ghost: false,
};

let emptyResult = (pos, name, isLexical) => (R.Leaf((name, ""), "", locForOffs(pos, pos)), false);

let mergeErrors = ((i1, errs1), (i2, errs2)) =>
  if (i1 == i2) {
    (i1, List.concat([errs1, errs2]))
  } else if (i1 < i2) {
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












let rec skipWhite = (i, text, len, ignoreNewlines) =>
  if (i >= len) {
    i
  } else {
    switch (text.[i]) {
    | ' ' => skipWhite(i + 1, text, len, ignoreNewlines)
    | '\t' => skipWhite(i + 1, text, len, ignoreNewlines)
    | '\n' when ignoreNewlines => {
      incLine(i + 1);
      skipWhite(i + 1, text, len, ignoreNewlines)
    }
    | _ => i
    }
  };

let skipALineComment = (i, start, text, len) => {
  let sl = String.length(start);
  /* TODO maybe iterate? */
  if (sl + i < len && String.sub(text, i, sl) == start) {
    try ({
      let l = String.index_from(text, i, '\n');
      incLine(l + 1);
      l + 1
    }) {
    | Not_found => len /* go to end */
    }
  } else {
    i
  }
};

let skipABlockComment = (i, (first, last), text, len) => {
  let fl = String.length(first);
  /* this might be a lot faster with a regex */
  if (fl + i < len && String.sub(text, i, fl) == first) {
    let fc = last.[0];
    let ll = String.length(last);
    let rec loop = (i) =>
      if (i + ll >= len) {
        failwith("Unterminated comment")
      } else if (text.[i] == fc && String.sub(text, i, ll) == last) {
        i + ll
      } else {
        if (text.[i] == '\n') {
          incLine(i + 1)
        };
        loop(i + 1)
      };
    loop(i)
  } else {
    i
  }
};

let optOr = (orr, opt) =>
  switch opt {
  | Some(x) => x
  | None => orr
  };

/* If we're skipping line comments, we can also skip newlines */
let rec skipLineComments = (i, start, text, len) => {
  let i' = skipALineComment(i, start, text, len);
  if (i' == len || i == i') {
    i'
  } else {
    let i'' = skipWhite(i', text, len, true);
    if (i'' > i') {
      skipLineComments(i'', start, text, len)
    } else {
      i''
    }
  }
};

let rec skipBlockComments = (i, ends, text, len, skipNewlines) => {
  let i' = skipABlockComment(i, ends, text, len);
  if (i' == len || i == i') {
    i'
  } else {
    let i'' = skipWhite(i', text, len, skipNewlines);
    if (i'' > i') {
      skipBlockComments(i'', ends, text, len, skipNewlines)
    } else {
      i''
    }
  }
};

let rec skipBlockAndLineComments = (i, ends, line, text, len) => {
  let i' = skipABlockComment(i, ends, text, len);
  let i' = skipWhite(i', text, len, true);
  let i' = skipALineComment(i', line, text, len);
  if (i' == i) {
    i'
  } else {
    skipBlockAndLineComments(i', ends, line, text, len)
  }
};

let skipAllWhite = (i, grammar, input, len) => {
  let i = skipWhite(i, input, len, true);
  let i' =
    switch (grammar.P.blockComment, grammar.P.lineComment) {
    | (Some(x), None) => skipBlockComments(i, x, input, len, true)
    | (Some(x), Some(y)) =>
      skipBlockAndLineComments(i, x, y, input, len)
    | (None, Some(x)) => skipLineComments(i, x, input, len)
    | (None, None) => i
    };
  i'
};

open PackCore.T;

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
    | [] => ((-1), emptyResult(i, rulename, isLexical), prevErrors)
    | [(sub_name, comment, rs), ...otherChoices] =>
      let rec loop = (i, items, path, loopIndex, isNegated) => {
        /* If in a NonLexical context, skip whitespace before trying to match a rule
         * Unless there are no more items. */
        let (i, items) =
          if (isLexical || items == []) {
            (i, items)
          } else {
            switch items {
            | [P.Lexify(p), ...rest] => (i, [p, ...rest])
            | _ =>
              let i = skipWhite(i, state.input, state.len, ignoringNewlines);
              let i' =
                switch (ignoringNewlines, grammar.P.blockComment, grammar.P.lineComment) {
                | (false, Some(x), _) => skipBlockComments(i, x, state.input, state.len, false)
                | (true, Some(x), None) => skipBlockComments(i, x, state.input, state.len, true)
                | (true, Some(x), Some(y)) =>
                  skipBlockAndLineComments(i, x, y, state.input, state.len)
                | (true, None, Some(x)) => skipLineComments(i, x, state.input, state.len)
                | (false, None, Some(_))
                | (_, None, None) => i
                };
              /* Printf.printf "Skipped comments %d %d\n" i i'; */
              (i', items)
            }
          };
        switch items {
        | [P.Empty, ...rest] => loop(i, rest, path, loopIndex + 1, isNegated)
        | [P.NoSpaceAfter(p), ...rest]
        | [P.NoSpaceBefore(p), ...rest]
        | [P.Lexify(p), ...rest] => loop(i, [p, ...rest], path, loopIndex, isNegated)
        | [P.Lookahead(p), ...rest] =>
          let (i', _, err) = loop(i, [p], path, loopIndex + 1, isNegated);
          if (i' >= i) {
            loop(i, rest, path, loopIndex + 1, isNegated /* propagate errors */)
          } else {
            ((-1), [], err)
          }
        | [P.Group(g), ...rest] => loop(i, List.concat([g, rest]), path, loopIndex, isNegated)
        | [P.Not(p), ...rest] =>
          let (i', _, err) = loop(i, [p], [RP.Item(P.Not(p), loopIndex), ...path], 0, ! isNegated);
          if (i' >= i) {
            ((-1), [], err)
          } else {
            loop(i, rest, path, loopIndex + 1, isNegated /* propagate errors */)
          }
        | [P.CommentEOL as item, ...rest] =>
          switch grammar.P.lineComment {
          | None =>
            if (i >= state.len || state.input.[i] == '\n') {
              let i' = skipWhite(i, state.input, state.len, true);
              let (i'', children, rest_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
              /* TODO collect comments */
              (i'', children, rest_errs)
            } else {
              ((-1), [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
            }
          | Some(lineComment) =>
            let i' = skipLineComments(i, lineComment, state.input, state.len);
            let i' =
              if (i' > i || i' >= state.len || state.input.[i] != '\n') {
                i'
              } else
                {
                  let i' = skipWhite(i', state.input, state.len, true);
                  let i' = skipLineComments(i', lineComment, state.input, state.len);
                  i'
                };
                /* i' + 1 */
            if (i' > i || i' >= state.len) {
              let (i'', children, rest_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
              /* TODO collect comments */
              (i'', children, rest_errs)
            } else {
              (
                /* Printf.printf "No actual skippage %d %d \"%s\"\n" i i' (String.sub state.input i 10); */
                (-1),
                [],
                (i, [(true, [RP.Item(item, loopIndex), ...path])])
              )
            }
          }
        | [P.NonTerminal(n, label) as item, ...rest] =>
          /* Printf.eprintf "[%s]> %s : %d\n" rulename n i; */
          let (i', (result, passThrough), errs) =
            PackCore.apply_rule(
              ~env={state, emptyResult, mergeErrors},
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
                | R.Leaf(_) => failwith("Passthrough can't have a leaf node")
                } :
                [(label |> optOr(""), result), ...children];
            (i'', children, mergeErrors(errs, rest_errs))
          } else {
            ((-1), [], errs)
          }
        | [P.Terminal(target_string, label) as item, ...rest] =>
          let slen = String.length(target_string);
          if (i + slen > state.len) {
            ((-1), [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
          } else {
            let sub = String.sub(state.input, i, slen);
            if (sub == target_string) {
              let (i'', children, err) =
                loop(i + slen, rest, path, loopIndex + 1, isNegated); /* TODO line / col num */
              let children =
                switch label {
                | Some(x) => [
                    (x, R.Leaf(("", target_string), target_string, locForOffs(i, i + slen))),
                    ...children
                  ]
                | None => children
                };
              (i'', children, err)
            } else {
              ((-1), [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
            }
          }
        | [P.Any(label) as item, ...rest] =>
          if (i >= state.len) {
            ((-1), [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
          } else {
            let (i'', children, err) = loop(i + 1, rest, path, loopIndex + 1, isNegated);
            let contents = String.sub(state.input, i, 1);
            let children =
              switch label {
              | Some(x) => [(x, R.Leaf(("", contents), contents, locForOffs(i, i + 1))), ...children]
              | None => children
              };
            (i'', children, err)
          }
        | [P.EOF, ...rest] =>
          if (i >= state.len) {
            (
              i,
              [],
              ((-1), []) /* TODO should I have a leaf here? */
            )
          } else {
            ((-1), [], (i, [(true, [RP.Item(P.EOF, loopIndex), ...path])]))
          }
        | [P.Chars(c1, c2, label) as item, ...rest] =>
          if (i >= state.len) {
            ((-1), [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
          } else if (state.input.[i] >= c1 && state.input.[i] <= c2) {
            let (i'', children, errs) = loop(i + 1, rest, path, loopIndex + 1, isNegated);
            let contents = String.sub(state.input, i, 1);
            let children =
              switch label {
              | Some(x) => [(x, R.Leaf(("", contents), contents, locForOffs(i, i + 1))), ...children]
              | None => children
              };
            (i'', children, errs)
          } else {
            ((-1), [], (i, [(true, [RP.Item(item, loopIndex), ...path])]))
          }
        | [P.Star(subr) as item, ...rest] =>
          let (i', subchildren, errs) =
            RuntimeUtils.greedy(~mergeErrors, loop, 0, None, subr, i, [RP.Item(item, loopIndex), ...path], 0, isNegated);
          if (i' >= i) {
            let (i'', children, more_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
            (i'', List.concat([subchildren, children]), mergeErrors(errs, more_errs))
          } else {
            ((-1), [], errs)
          }
        | [P.Plus(subr) as item, ...rest] =>
          let (i', subchildren, errs) =
            RuntimeUtils.greedy(~mergeErrors, loop, 1, None, subr, i, [RP.Item(item, loopIndex), ...path], 0, isNegated);
          if (i' >= i) {
            let (i'', children, more_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
            (i'', List.concat([subchildren, children]), mergeErrors(errs, more_errs))
          } else {
            ((-1), [], errs)
          }
        | [P.Optional(subr) as item, ...rest] =>
          let (i', subchildren, errs) =
            RuntimeUtils.greedy(~mergeErrors, loop, 0, Some(1), subr, i, [RP.Item(item, loopIndex), ...path], 0, isNegated);
          if (i' >= i) {
            let (i'', children, more_errs) = loop(i', rest, path, loopIndex + 1, isNegated);
            (i'', List.concat([subchildren, children]), mergeErrors(errs, more_errs))
          } else {
            ((-1), [], errs)
          }
        | [] => (i, [], ((-1), []))
        }
      };
      let subPath = numChoices === 1 ? path : [RP.Choice(choiceIndex, sub_name), ...path];

      /* TODO maybe I don't have to do this if I'm not capturing comments... */
      let precomments = if (capturesComments) {
        let p = pendingComments^;
        pendingComments := [];
        p
      } else {[]};

      let (i', children, err) = loop(i, rs, subPath, 0, isNegated);
      let errs = mergeErrors(prevErrors, err);
      /* Printf.eprintf "$$ %d [%d, %d] (%s - %d)\n" (fst errs) (fst prevErrors) (fst err) rulename choiceIndex; */
      /* Printf.eprintf "PARSE[%s:%d] %d) %s\n" rulename i (fst errs) (PackTypes.Error.errorsText (snd errs)); */
      if (i' >= i) {
        /* Printf.eprintf "<final>\n"; */
        let name = (rulename, sub_name);
        let loc = locForOffs(i, i');
        let result = (
          leaf ? R.Leaf(name, String.sub(state.input, i, i' - i), loc) : {
            R.Node(name, children, loc, capturesComments
            ? {
              let d = currentDocComment^;
              currentDocComment := None;
              let innerComments = pendingComments^;
              pendingComments := [];
              /* TODO EOLComments */
              Some((d, precomments, innerComments, None))
            }
            : None)
          },
          passThrough
        );
        /* let children = leaf ? [] : children; */
        /* Printf.printf "match %s \"%s\" [%d..%d]\n" rulename name i (i' - 1); */
        /* let typ = isLexical ? (Lexical (rulename, sub_name, choiceIndex)  passThrough) : Nonlexical (rulename, sub_name, choiceIndex) passThrough; */
        (i', result, errs)
      } else {
        if (capturesComments) {
          pendingComments := precomments;
        };
        /* Printf.eprintf "<nother choice>\n"; */
        process(otherChoices, errs, choiceIndex + 1)
      }
    };
  process(choices, ((-1), []), 0)
  /*** TODO if wasIgnoringNewlines == false && ignoringNewlines = true, then ignore any trailing newlines */
};

let parse = (grammar: PackTypes.Parsing.grammar, start, input) => {
  startFile("File name");
  let state = PackCore.initialState(input);
  /* TODO ignoringNewlines should be configurable? */
  let (i, (result, _), errs) =
    PackCore.apply_rule(
      ~env={state, emptyResult, mergeErrors},
      ~parse=parse(grammar),
      start,
      0,
      false,
      false,
      [],
    );
  let i = i >=0 ? skipAllWhite(i, grammar, input, String.length(input)) : i;
  if (i == (-1)) {
    Belt.Result.Error((None, (0, posForLoc(fst(errs)), errs)))
  } else if (i < state.len) {
    Belt.Result.Error((Some(result), (i, posForLoc(fst(errs)), errs)))
  } else {
    Belt.Result.Ok(result)
  }
};
