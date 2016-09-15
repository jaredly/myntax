/* from https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/ with modifications */
let module P = PackTypes.Parsing;
let module R = PackTypes.Result;
let module RP = PackTypes.Path;

/* Parser.
 * Packrat parser with left recursion, see:
 * "Packrat Parsers Can Support Left Recursion"
 * Alessandro Warth, James R. Douglass, Todd Millstein
 */
let module StringSet = Set.Make String;

type lr = {mutable seed: ans, mutable rulename: string, mutable head: option head}
and memoentry = {mutable ans: ans_or_lr, mutable pos: int}
and ans_or_lr = | Answer ans | LR lr
and ans = (int, (PackTypes.Result.result, bool), PackTypes.Error.errors)
and head = {
  mutable hrule: string,
  mutable involved_set: StringSet.t,
  mutable eval_set: StringSet.t
};

exception Found ans;

let emptyResult pos name isLexical => (R.Leaf (name, "") "" (pos, pos), false);
 /* {
  R.start: pos,
  cend: pos,
  typ: isLexical ? Lexical (name, "", 0) "" false : Nonlexical (name, "", 0) false,
  label: None,
  children: [],
}; */

type state = {
  mutable lrstack: list lr,
  memo: Hashtbl.t (StringSet.elt, int) memoentry,
  heads: Hashtbl.t int head,
  mutable cpos: int,
  len: int,
  input: string,
};

let unwrap opt =>
  switch opt {
    | Some x => x
    | None => failwith "Expected Some(x)"
  };

let fst (a, _, _) => a;

let rec skipWhite i text len ignoreNewlines => {
  if (i >= len) {
    i
  } else {
    switch (String.get text i) {
      | ' ' => skipWhite (i + 1) text len ignoreNewlines
      | '\t' => skipWhite (i + 1) text len ignoreNewlines
      | '\n' when ignoreNewlines => skipWhite (i + 1) text len ignoreNewlines
      | _ => i
    }
  }
};

let skipALineComment i start text len => {
  let sl = String.length start;
  /* TODO maybe iterate? */
  if (sl + i < len && String.sub text i sl == start) {
    try {String.index_from text i '\n'}
    { | Not_found => len /* go to end */ };
  } else {
    i
  }
};

let skipABlockComment i (first, last) text len => {
  let fl = String.length first;
  /* this might be a lot faster with a regex */
  if (fl + i < len && String.sub text i fl == first) {
    let fc = String.get last 0;
    let ll = String.length last;
    let rec loop i => {
      if (i + ll >= len) {
        failwith "Unterminated comment"
      } else if (String.get text i == fc && String.sub text i ll == last) {
        i + ll;
      } else {
        loop (i + 1)
      }
    };
    loop i
  } else {
    i
  }
};

let optOr orr opt => switch opt { | Some x => x | None => orr};

/* If we're skipping line comments, we can also skip newlines */
let rec skipLineComments i start text len => {
  let i' = skipALineComment i start text len;
  if (i' == len || i == i') {
    i'
  } else {
    let i'' = (skipWhite i' text len true);
    if (i'' > i') {
      skipLineComments i'' start text len
    } else {
      i''
    }
  }
};

let rec skipBlockComments i ends text len skipNewlines => {
  let i' = skipABlockComment i ends text len;
  if (i' == len || i == i') {
    i'
  } else {
    let i'' = (skipWhite i' text len skipNewlines);
    if (i'' > i') {
      skipBlockComments i'' ends text len skipNewlines
    } else {
      i''
    }
  }
};

let rec skipBlockAndLineComments i ends line text len => {
  let i' = skipABlockComment i ends text len;
  let i' = skipWhite i' text len true;
  let i' = skipALineComment i' line text len;
  if (i' == i) {
    i'
  } else {
    skipBlockAndLineComments i' ends line text len;
  }
};

let mergeErrs (i1, errs1) (i2, errs2) => {
  if (i1 == i2) {
    (i1, List.concat [errs1, errs2])
  } else if (i1 < i2) {
    (i2, errs2)
  } else {
    (i1, errs1)
  }
};

let rec greedy loop min max subr i path greedyCount isNegated => {
  /* implements e* e+ e? */
  switch max {
    | Some 0 => (i, [], (-1, []))
    | _ =>
    if (min > 0) {
      /* we must match at least min or fail */
      let (i', found, err) = loop i [subr] [RP.Iter greedyCount, ...path] 0 isNegated;
      if (i' >= i) {
        let (i'', children, merr) = greedy loop (min - 1) max subr i' path (greedyCount + 1) isNegated;
        (i'', List.concat [found, children], mergeErrs err merr)
      } else {
        (-1, [], err)
      }
    } else {
      /* try matching, doesn't matter if we fail */
      let (i', children, err) = loop i [subr] [RP.Iter greedyCount, ...path] 0 isNegated;
      if (i' >= i) {
        let max =
        switch max {
          | None => None
          | Some n => Some (n - 1)
        };
        let (i'', more, merr) = greedy loop 0 max subr i' path (greedyCount + 1) isNegated;
        (i'', List.concat [children, more], mergeErrs err merr)
      } else {
        (i, [], err) /* don't fail, return longest match */
      }
    }
  }
};

/* Apply rule 'rulename' at position 'i' in the input.  Returns the new
 * position if the rule can be applied, else -1 if fails.
 */
let rec apply_rule grammar state rulename i ignoringNewlines isNegated path => {
  /* print_endline ("Apply rule" ^ rulename); */
  let isLexical = (Char.uppercase (String.get rulename 0)) != (String.get rulename 0);
  switch (recall grammar state rulename i isLexical ignoringNewlines isNegated path) {
  | None =>
    let lr = {seed: (-1, emptyResult i rulename isLexical, (-1, [])), rulename, head: None};
    state.lrstack = [lr, ...state.lrstack];
    let memoentry = {ans: LR lr, pos: i};
    Hashtbl.add state.memo (rulename, i) memoentry;
    let answer = parse grammar state rulename i isLexical ignoringNewlines isNegated path;
    state.lrstack = List.tl state.lrstack;
    memoentry.pos = state.cpos;
    if (lr.head != None) {
      lr.seed = answer;
      lr_answer grammar state rulename i memoentry isLexical ignoringNewlines isNegated path
    } else {
      memoentry.ans = Answer answer;
      answer
    }
  | Some memoentry =>
    state.cpos = memoentry.pos;
    switch memoentry.ans {
    | LR lr =>
      setup_lr state rulename lr;
      lr.seed
    | Answer answer => answer
    }
  }
}

and setup_lr state rulename lr => {
  if (lr.head == None) {
    lr.head = Some {hrule: rulename, involved_set: StringSet.empty, eval_set: StringSet.empty}
  };
  let lr_head = unwrap lr.head;
  let rec loop =
    fun
    | [] => assert false
    | [l, ..._] when l.head == Some lr_head => ()
    | [l, ...ls] => {
        l.head = Some lr_head;
        lr_head.involved_set = StringSet.add l.rulename lr_head.involved_set;
        loop ls
      };
  loop state.lrstack
}

and lr_answer grammar state rulename i memoentry isLexical ignoringNewlines isNegated path => {
  let lr =
    switch memoentry.ans {
    | Answer _ => assert false
    | LR lr => lr
    };
  let head =
    switch lr.head {
    | None => assert false
    | Some head => head
    };
  if (head.hrule != rulename) {
    lr.seed
  } else {
    memoentry.ans = Answer lr.seed;
    if (fst lr.seed == -1) {
      (-1, emptyResult i rulename isLexical, (-1, []))
    } else {
      grow_lr grammar state rulename i memoentry head isLexical ignoringNewlines isNegated path
    }
  }
}

and recall grammar state rulename i isLexical ignoringNewlines isNegated path => {
  let maybeEntry =
    try (Some (Hashtbl.find state.memo (rulename, i))) {
    | Not_found => None
    };
  let maybeHead =
    try (Some (Hashtbl.find state.heads i)) {
    | Not_found => None
    };
  switch maybeHead {
  | None => maybeEntry
  | Some head =>
    if (maybeEntry == None && not (StringSet.mem rulename (StringSet.add head.hrule head.involved_set))) {
      Some {ans: Answer (-1, emptyResult i rulename isLexical, (-1, [])), pos: i}
    } else {
      if (StringSet.mem rulename head.eval_set) {
        head.eval_set = StringSet.remove rulename head.eval_set;
        let answer = parse grammar state rulename i isLexical ignoringNewlines isNegated path;
        /* Original paper RECALL function seems to have a bug ... */
        let memoentry = unwrap maybeEntry;
        memoentry.ans = Answer answer;
        memoentry.pos = state.cpos
      };
      maybeEntry
    }
  }
}

and grow_lr grammar state rulename i memoentry head isLexical ignoringNewlines isNegated path => {
  Hashtbl.replace state.heads i head; /* A */
  let rec loop () => {
    state.cpos = i;
    head.eval_set = head.involved_set; /* B */
    let ans = parse grammar state rulename i isLexical ignoringNewlines isNegated path;
    let oans = switch (memoentry.ans) {
      | Answer (i, _, _) => i
      | LR _ => -1
    };
    if (fst ans == -1 || (state.cpos <= memoentry.pos && fst ans <= oans)) {
      ()
    } else {
      memoentry.ans = Answer ans;
      memoentry.pos = state.cpos;
      loop ()
    }
  };
  loop ();
  Hashtbl.remove state.heads i; /* C */
  state.cpos = memoentry.pos;
  switch memoentry.ans {
    | Answer answer => answer
    | LR _ => assert false
  }
}

and parse grammar state rulename i isLexical ignoringNewlines isNegated path => {
  /* Printf.printf "parse %s %d\n" rulename i; */
  let {P.ignoreNewlines, choices, passThrough, leaf} =
    try (List.assoc rulename grammar.P.rules) {
    | Not_found =>
      Printf.eprintf "error in grammar: unknown rulename '%s'\n" rulename;
      exit 1
    };
  let wasIgnoringNewlines = ignoringNewlines;
  let ignoringNewlines = switch (ignoreNewlines, ignoringNewlines) {
    | (Inherit, x) => x
    | (No, _) => false
    | (Yes, _) => true
  };
  let numChoices = List.length choices;
  /* Try each choice in turn until one matches. */
  let rec process choices prevErrors choiceIndex => {
    switch choices {
      | [] => (-1, emptyResult i rulename isLexical, prevErrors)
      | [(sub_name, comment, rs), ...otherChoices] => {
        let rec loop i items path loopIndex isNegated => {
          /* If in a NonLexical context, skip whitespace before trying to match a rule */
          let (i, items) = if isLexical {
            (i, items)
          } else {
            switch items {
              | [P.Lexify p, ...rest] => (i, [p, ...rest])
              | _ => {
                let i = skipWhite i state.input state.len ignoringNewlines;
                let i' = switch (ignoringNewlines, grammar.P.blockComment, grammar.P.lineComment) {
                  | (false, Some x, _) => skipBlockComments i x state.input state.len false
                  | (true, Some x, None) => skipBlockComments i x state.input state.len true
                  | (true, Some x, Some y) => skipBlockAndLineComments i x y state.input state.len
                  | (true, None, Some x) => skipLineComments i x state.input state.len
                  | (false, None, Some _)
                  | (_, None, None) => i
                };
                /* Printf.printf "Skipped comments %d %d\n" i i'; */
                (i', items)
              }
            }
          };

          switch items {
            | [P.Lexify p, ...rest] => loop i [p, ...rest] path loopIndex isNegated
            | [P.Empty, ...rest] => loop i rest path (loopIndex + 1) isNegated
            | [P.Lookahead p, ...rest] => {
              let (i', _, err) = loop i [p] path (loopIndex + 1) isNegated;
              if (i' >= i) {
                loop i rest path (loopIndex + 1) isNegated/* propagate errors */
              } else {
                (-1, [], err)
              }
            }

            | [P.Group g, ...rest] => loop i (List.concat [g, rest]) path loopIndex isNegated
            | [P.Not p, ...rest] => {
              let (i', _, err) = loop i [p] [RP.Item (Not p) loopIndex, ...path] 0 (not isNegated);
              if (i' >= i) {
                (-1, [], err)
              } else {
                loop i rest path (loopIndex + 1) isNegated/* propagate errors */
              }
            }

            | [P.CommentEOL as item, ...rest] => {
              let i' = skipLineComments i (unwrap grammar.lineComment) state.input state.len;
              let i' = if (i' > i || i' >= state.len || String.get state.input i != '\n') {
                i'
              } else {
                let i' = skipWhite i' state.input state.len true;
                let i' = skipLineComments i' (unwrap grammar.lineComment) state.input state.len;
                i'
                /* i' + 1 */
              };
              if (i' > i || i' >= state.len) {
                let (i'', children, rest_errs) = loop i' rest path (loopIndex + 1) isNegated;
                /* TODO collect comments */
                (i'', children, rest_errs)
              } else {
                /* Printf.printf "No actual skippage %d %d \"%s\"\n" i i' (String.sub state.input i 10); */
                (-1, [], (i, [(true, [RP.Item item loopIndex, ...path])]))
              }
            }

            | [(P.NonTerminal n label) as item, ...rest] => {
                let (i', (result, passThrough), errs) = apply_rule grammar state n i ignoringNewlines isNegated [RP.Item item loopIndex, ...path];
                if (i' >= i) {
                  let (i'', children, rest_errs) = loop i' rest path (loopIndex + 1) isNegated;
                  let children = passThrough ? (switch result {
                    | Node _ subchildren _ => List.concat [subchildren, children]
                    | Leaf _ => failwith "Passthrough can't have a leaf node"
                  }) : [(label |> optOr "", result), ...children];
                  (i'', children, mergeErrs errs rest_errs)
                } else {
                  (-1, [], errs)
                }
              }

            | [(P.Terminal target_string label) as item, ...rest] => {
                let slen = String.length target_string;
                if (i + slen > state.len) {
                  (-1, [], (i, [(true, [RP.Item item loopIndex, ...path])]))
                } else {
                  let sub = String.sub state.input i slen;
                  if (sub == target_string) {
                    let (i'', children, err) = loop (i + slen) rest path (loopIndex + 1) isNegated; /* TODO line / col num */
                    let children = switch label {
                      | Some x => [(x, R.Leaf ("", target_string) target_string (i, i + slen)), ...children]
                      | None => children
                    };
                    (i'', children, err)
                  } else {
                    (-1, [], (i, [(true, [RP.Item item loopIndex, ...path])]))
                  }
                }
              }

            | [(P.Any label) as item, ...rest] =>
              if (i >= state.len) {
                (-1, [], (i, [(true, [RP.Item item loopIndex, ...path])]))
              } else {
                let (i'', children, err) = loop (i + 1) rest path (loopIndex + 1) isNegated;
                let contents = (String.sub state.input i 1);
                let children = switch label {
                  | Some x => [(x, R.Leaf ("", contents) contents (i, i + 1)), ...children]
                  | None => children
                };
                (i'', children, err)
              }

            | [P.EOF, ...rest] => {
              if (i >= state.len) {
                (i, [], (-1, [])) /* TODO should I have a leaf here? */
              } else {
                (-1, [], (i, [(true, [RP.Item EOF loopIndex, ...path])]))
              }
            }

            | [(P.Chars c1 c2 label) as item, ...rest] =>
              if (i >= state.len) {
                (-1, [], (i, [(true, [RP.Item item loopIndex, ...path])]))
              } else if (state.input.[i] >= c1 && state.input.[i] <= c2) {
                let (i'', children, errs) = loop (i + 1) rest path (loopIndex + 1) isNegated;
                let contents = (String.sub state.input i 1);
                let children = switch label {
                  | Some x => [(x, R.Leaf ("", contents) contents (i, i + 1)), ...children]
                  | None => children
                };
                (i'', children, errs)
              } else {
                (-1, [], (i, [(true, [RP.Item item loopIndex, ...path])]))
              }

            | [(P.Star subr _) as item, ...rest] => {
                let (i', subchildren, errs) = greedy loop 0 None subr i [RP.Item item loopIndex, ...path] 0 isNegated;
                if (i' >= i) {
                  let (i'', children, more_errs) = loop i' rest path (loopIndex + 1) isNegated;
                  (i'', List.concat [subchildren, children], mergeErrs errs more_errs)
                } else {
                  (-1, [], errs)
                }
              }

            | [(P.Plus subr _) as item, ...rest] => {
                let (i', subchildren, errs) = greedy loop 1 None subr i [RP.Item item loopIndex, ...path] 0 isNegated;
                if (i' >= i) {
                  let (i'', children, more_errs) = loop i' rest path (loopIndex + 1) isNegated;
                  (i'', List.concat [subchildren, children], mergeErrs errs more_errs)
                } else {
                  (-1, [], errs)
                }
              }

            | [(P.Optional subr _) as item, ...rest] => {
                let (i', subchildren, errs) = greedy loop 0 (Some 1) subr i [RP.Item item loopIndex, ...path] 0 isNegated;
                if (i' >= i) {
                  let (i'', children, more_errs) = loop i' rest path (loopIndex + 1) isNegated;
                  (i'', List.concat [subchildren, children], mergeErrs errs more_errs)
                } else {
                  (-1, [], errs)
                }
              }
            | [] => (i, [], (-1, []))
          }
        }
        ;

        let subPath = numChoices === 1 ? path : [RP.Choice choiceIndex sub_name, ...path];
        let (i', children, err) = loop i rs subPath 0 isNegated;
        let errs = mergeErrs prevErrors err;
        if (i' >= i) {
          let name = (rulename, sub_name);
          let loc = (i, i');
          let result = ((leaf ? R.Leaf name (String.sub state.input i (i' - i)) loc : R.Node name children loc), passThrough);
          /* let children = leaf ? [] : children; */
          /* Printf.printf "match %s \"%s\" [%d..%d]\n" rulename name i (i' - 1); */
          /* let typ = isLexical ? (Lexical (rulename, sub_name, choiceIndex)  passThrough) : Nonlexical (rulename, sub_name, choiceIndex) passThrough; */
          (i', result, errs)
        } else {
          process otherChoices errs (choiceIndex + 1)
        }
      }
    }
  };
  process choices (-1, []) 0
  /** TODO if wasIgnoringNewlines == false && ignoringNewlines = true, then ignore any trailing newlines */
};

let initialState input => {
  lrstack: [],
  cpos: 0,
  memo: Hashtbl.create 13,
  heads: Hashtbl.create 13,
  len: String.length input,
  input,
};

let parse (grammar: PackTypes.Parsing.grammar) start input => {
  let state = initialState input;
  /* TODO ignoringNewlines should be configurable? */
  let (i, (result, _), errs) = apply_rule grammar state start 0 false false [];
  if (i == -1) {
    R.Failure None (0, errs)
  } else if (i < state.len) {
    R.Failure (Some result) (i, errs)
  } else {
    R.Success result
  }
};
