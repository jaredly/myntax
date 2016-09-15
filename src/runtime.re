/* from https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/ with modifications */
open PackTypes;

/* Parser.
 * Packrat parser with left recursion, see:
 * "Packrat Parsers Can Support Left Recursion"
 * Alessandro Warth, James R. Douglass, Todd Millstein
 */
let module StringSet = Set.Make String;

type lr = {mutable seed: ans, mutable rulename: string, mutable head: option head}
and memoentry = {mutable ans: ans_or_lr, mutable pos: int}
and ans_or_lr = | Answer ans | LR lr
and ans = (int, result, PackTypes.Result.errors)
and head = {
  mutable hrule: string,
  mutable involved_set: StringSet.t,
  mutable eval_set: StringSet.t
};

exception Found ans;

let emptyResult pos name isLexical => {
  start: pos,
  cend: pos,
  typ: isLexical ? Lexical name "" false : Nonlexical name false,
  label: None,
  children: [],
};

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
      | '\n' when ignoreNewlines => skipWhite (i + 1) text len ignoreNewlines
      | _ => i
    }
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

let rec greedy loop min max subr i path greedyCount => {
  /* implements e* e+ e? */
  switch max {
    | Some 0 => (i, [], (-1, []))
    | _ =>
    if (min > 0) {
      /* we must match at least min or fail */
      let (i', found, err) = loop i [subr] [Iter greedyCount, ...path] 0;
      if (i' >= i) {
        let (i'', children, merr) = greedy loop (min - 1) max subr i' path (greedyCount + 1);
        (i'', List.concat [found, children], mergeErrs err merr)
      } else {
        (-1, [], err)
      }
    } else {
      /* try matching, doesn't matter if we fail */
      let (i', children, err) = loop i [subr] [Iter greedyCount, ...path] 0;
      if (i' >= i) {
        let max =
        switch max {
          | None => None
          | Some n => Some (n - 1)
        };
        let (i'', more, merr) = greedy loop 0 max subr i' path (greedyCount + 1);
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
let rec apply_rule grammar state rulename i ignoringNewlines path => {
  /* print_endline ("Apply rule" ^ rulename); */
  let isLexical = (Char.uppercase (String.get rulename 0)) != (String.get rulename 0);
  switch (recall grammar state rulename i isLexical ignoringNewlines path) {
  | None =>
    let lr = {seed: (-1, emptyResult i rulename isLexical, (-1, [])), rulename, head: None};
    state.lrstack = [lr, ...state.lrstack];
    let memoentry = {ans: LR lr, pos: i};
    Hashtbl.add state.memo (rulename, i) memoentry;
    let answer = parse grammar state rulename i isLexical ignoringNewlines path;
    state.lrstack = List.tl state.lrstack;
    memoentry.pos = state.cpos;
    if (lr.head != None) {
      lr.seed = answer;
      lr_answer grammar state rulename i memoentry isLexical ignoringNewlines path
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

and lr_answer grammar state rulename i memoentry isLexical ignoringNewlines path => {
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
      grow_lr grammar state rulename i memoentry head isLexical ignoringNewlines path
    }
  }
}

and recall grammar state rulename i isLexical ignoringNewlines path => {
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
        let answer = parse grammar state rulename i isLexical ignoringNewlines path;
        /* Original paper RECALL function seems to have a bug ... */
        let memoentry = unwrap maybeEntry;
        memoentry.ans = Answer answer;
        memoentry.pos = state.cpos
      };
      maybeEntry
    }
  }
}

and grow_lr grammar state rulename i memoentry head isLexical ignoringNewlines path => {
  Hashtbl.replace state.heads i head; /* A */
  let rec loop () => {
    state.cpos = i;
    head.eval_set = head.involved_set; /* B */
    let ans = parse grammar state rulename i isLexical ignoringNewlines path;
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

and parse grammar state rulename i isLexical ignoringNewlines path => {
  /* Printf.printf "parse %s %d\n" rulename i; */
  let {ignoreNewlines, choices, passThrough} =
    try (List.assoc rulename grammar) {
    | Not_found =>
      Printf.eprintf "error in grammar: unknown rulename '%s'\n" rulename;
      exit 1
    };
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
        let rec loop i items path loopIndex => {
          /* If in a NonLexical context, skip whitespace before trying to match a rule */
          let (i, items) = if isLexical {
            (i, items)
          } else {
            switch items {
              | [Lexify p, ...rest] => (i, [p, ...rest])
              | _ => (skipWhite i state.input state.len ignoringNewlines, items)
            }
          };

          switch items {
            | [Lexify p, ...rest] => loop i [p, ...rest] path loopIndex
            | [Empty, ...rest] => loop i rest path (loopIndex + 1)
            | [Lookahead p, ...rest] => {
              let (i', _, err) = loop i [p] path (loopIndex + 1);
              if (i' >= i) {
                loop i rest path (loopIndex + 1)/* propagate errors */
              } else {
                (-1, [], err)
              }
            }

            | [Group g, ...rest] => loop i (List.concat [g, rest]) path loopIndex
            | [Not p, ...rest] => {
              let (i', _, err) = loop i [p] [Item (Not p) loopIndex, ...path] 0;
              if (i' >= i) {
                (-1, [], err)
              } else {
                loop i rest path (loopIndex + 1)/* propagate errors */
              }
            }

            | [(NonTerminal n label) as item, ...rest] => {
                let (i', result, errs) = apply_rule grammar state n i ignoringNewlines [Item item loopIndex, ...path];
                if (i' >= i) {
                  let (i'', children, rest_errs) = loop i' rest path (loopIndex + 1);
                  (i'', [{...result, label}, ...children], mergeErrs errs rest_errs)
                } else {
                  (-1, [], errs)
                }
              }

            | [(Terminal target_string label) as item, ...rest] => {
                let slen = String.length target_string;
                if (i + slen > state.len) {
                  (-1, [], (i, [(true, [Item item loopIndex, ...path])]))
                } else {
                  let sub = String.sub state.input i slen;
                  if (sub == target_string) {
                    let (i'', children, err) = loop (i + slen) rest path (loopIndex + 1);
                    /* TODO line / col num */
                    (i'', [{label, start: i, cend: i + slen, children: [], typ: Terminal sub}, ...children], err)
                  } else {
                    (-1, [], (i, [(true, [Item item loopIndex, ...path])]))
                  }
                }
              }

            | [(Any label) as item, ...rest] =>
              if (i >= state.len) {
                (-1, [], (i, [(true, [Item item loopIndex, ...path])]))
              } else {
                let (i'', children, err) = loop (i + 1) rest path (loopIndex + 1);
                (i'', [{label, start: i, cend: i + 1, children: [], typ: Terminal (String.sub state.input i 1)}, ...children], err)
              }

            | [EOF, ...rest] => {
              if (i >= state.len) {
                (i, [{label: None, start: i, cend: i, children: [], typ: Terminal ""}], (-1, []))
              } else {
                (-1, [], (i, [(true, [Item EOF loopIndex, ...path])]))
              }
            }

            | [(Chars c1 c2 label) as item, ...rest] =>
              if (i >= state.len) {
                (-1, [], (i, [(true, [Item item loopIndex, ...path])]))
              } else if (state.input.[i] >= c1 && state.input.[i] <= c2) {
                let (i'', children, errs) = loop (i + 1) rest path (loopIndex + 1);
                (i'', [{label, start: i, cend: i + 1, children: [], typ: Terminal (String.sub state.input i 1)}, ...children], errs)
              } else {
                (-1, [], (i, [(true, [Item item loopIndex, ...path])]))
              }

            | [(Star subr label) as item, ...rest] => {
                let (i', subchildren, errs) = greedy loop 0 None subr i [Item item loopIndex, ...path] 0;
                if (i' >= i) {
                  let (i'', children, more_errs) = loop i' rest path (loopIndex + 1);
                  (i'', [{label, start: i, cend: i', children: subchildren, typ: Iter}, ...children], mergeErrs errs more_errs)
                } else {
                  (-1, [], errs)
                }
              }

            | [(Plus subr label) as item, ...rest] => {
                let (i', subchildren, errs) = greedy loop 1 None subr i [Item item loopIndex, ...path] 0;
                if (i' >= i) {
                  let (i'', children, more_errs) = loop i' rest path (loopIndex + 1);
                  (i'', [{label, start: i, cend: i', children: subchildren, typ: Iter}, ...children], mergeErrs errs more_errs)
                } else {
                  (-1, [], errs)
                }
              }

            | [(Optional subr label) as item, ...rest] => {
                let (i', subchildren, errs) = greedy loop 0 (Some 1) subr i [Item item loopIndex, ...path] 0;
                if (i' >= i) {
                  let (i'', children, more_errs) = loop i' rest path (loopIndex + 1);
                  (i'', [{label, start: i, cend: i', children: subchildren, typ: Iter}, ...children], mergeErrs errs more_errs)
                } else {
                  (-1, [], errs)
                }
              }
            | [] => (i, [], (-1, []))
          }
        }
        ;

        let subPath = numChoices === 1 ? path : [Choice choiceIndex sub_name, ...path];
        let (i', children, err) = loop i rs subPath 0;
        let errs = mergeErrs prevErrors err;
        if (i' >= i) {
          /* Printf.printf "match %s \"%s\" [%d..%d]\n" rulename name i (i' - 1); */
          let name = if (numChoices === 1) { rulename } else {(rulename ^ "_" ^ sub_name)};
          let typ = isLexical ? (Lexical name (String.sub state.input i (i' - i)) passThrough) : Nonlexical name passThrough;
          (i', {start: i, cend: i', children, label: None, typ}, errs)
        } else {
          process otherChoices errs (choiceIndex + 1)
        }
      }
    }
  };
  process choices (-1, []) 0
};

let initialState input => {
  lrstack: [],
  cpos: 0,
  memo: Hashtbl.create 13,
  heads: Hashtbl.create 13,
  len: String.length input,
  input,
};

let parse (grammar: PackTypes.grammar) start input => {
  let state = initialState input;
  /* TODO ignoringNewlines should be configurable? */
  let (i, result, errs) = apply_rule grammar state start 0 false [];
  if (i == -1) {
    Failure None (0, errs)
  } else if (i < state.len) {
    Failure (Some result) (i, errs)
  } else {
    Success result
  }
};
