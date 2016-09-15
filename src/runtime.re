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
and ans = (int, result)
and head = {
  mutable hrule: string,
  mutable involved_set: StringSet.t,
  mutable eval_set: StringSet.t
};

exception Found ans;

let emptyState pos name isLexical => {
  start: pos,
  cend: pos,
  typ: isLexical ? Lexical name "" : Nonlexical name,
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

let optionGet v =>
  switch v {
    | Some x => x
    | None => failwith "Expected Some(x)"
  };

let fst (a, _) => a;

let rec skipWhite i text len => {
  if (i >= len) {
    i
  } else if (String.get text i === ' ') {
    skipWhite (i + 1) text len
  } else {
    i
  }
};

/* Apply rule 'rulename' at position 'i' in the input.  Returns the new
 * position if the rule can be applied, else -1 if fails.
 */
let rec apply_rule grammar state rulename i => {
  /* print_endline ("Apply rule" ^ rulename); */
  let isLexical = (Char.uppercase (String.get rulename 0)) != (String.get rulename 0);
  switch (recall grammar state rulename i isLexical) {
  | None =>
    let lr = {seed: (-1, emptyState i rulename isLexical), rulename, head: None};
    state.lrstack = [lr, ...state.lrstack];
    let m = {ans: LR lr, pos: i};
    Hashtbl.add state.memo (rulename, i) m;
    let r = parse grammar state rulename i isLexical;
    state.lrstack = List.tl state.lrstack;
    m.pos = state.cpos;
    if (lr.head != None) {
      lr.seed = r;
      lr_answer grammar state rulename i m isLexical
    } else {
      m.ans = Answer r;
      r
    }
  | Some m =>
    state.cpos = m.pos;
    switch m.ans {
    | LR lr =>
      setup_lr state rulename lr;
      lr.seed
    | Answer r => r
    }
  }
}

and setup_lr state rulename lr => {
  if (lr.head == None) {
    lr.head = Some {hrule: rulename, involved_set: StringSet.empty, eval_set: StringSet.empty}
  };
  let lr_head = optionGet lr.head;
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

and lr_answer grammar state rulename i m isLexical => {
  let lr =
    switch m.ans {
    | Answer _ => assert false
    | LR lr => lr
    };
  let h =
    switch lr.head {
    | None => assert false
    | Some h => h
    };
  if (h.hrule != rulename) {
    lr.seed
  } else {
    m.ans = Answer lr.seed;
    if (fst lr.seed == -1) {
      (-1, emptyState i rulename isLexical)
    } else {
      grow_lr grammar state rulename i m h isLexical
    }
  }
}

and recall grammar state rulename i isLexical => {
  let m =
    try (Some (Hashtbl.find state.memo (rulename, i))) {
    | Not_found => None
    };
  let h =
    try (Some (Hashtbl.find state.heads i)) {
    | Not_found => None
    };
  switch h {
  | None => m
  | Some h =>
    if (m == None && not (StringSet.mem rulename (StringSet.add h.hrule h.involved_set))) {
      Some {ans: Answer (-1, emptyState i rulename isLexical), pos: i}
    } else {
      if (StringSet.mem rulename h.eval_set) {
        h.eval_set = StringSet.remove rulename h.eval_set;
        let r = parse grammar state rulename i isLexical;
        /* Original paper RECALL function seems to have a bug ... */
        let m = optionGet m;
        m.ans = Answer r;
        m.pos = state.cpos
      };
      m
    }
  }
}

and grow_lr grammar state rulename i m h isLexical => {
  Hashtbl.replace state.heads i h; /* A */
  let rec loop () => {
    state.cpos = i;
    h.eval_set = h.involved_set; /* B */
    let ans = parse grammar state rulename i isLexical;
    let oans = switch (m.ans) {
      | Answer (i, _) => i
      | LR _ => -1
    };
    if (fst ans == -1 || (state.cpos <= m.pos && fst ans <= oans)) {
      ()
    } else {
      m.ans = Answer ans;
      m.pos = state.cpos;
      loop ()
    }
  };
  loop ();
  Hashtbl.remove state.heads i; /* C */
  state.cpos = m.pos;
  switch m.ans {
  | Answer r => r
  | LR _ => assert false
  }
}

and parse grammar state rulename i isLexical => {
  /* Printf.printf "parse %s %d\n" rulename i; */
  let choices =
    try (List.assoc rulename grammar) {
    | Not_found =>
      Printf.eprintf "error in grammar: unknown rulename '%s'\n" rulename;
      exit 1
    };
  /* Try each choice in turn until one matches. */
  try {
    List.iter
      (

        fun (sub_name, comment, rs) => {
          /* Printf.printf "parse %s \"%s\" %d\n" rulename name i; */
          let rec loop i items => {
            let i = if isLexical {
              i
            } else {
              switch items {
                | [Lexify p, ..._] => i
                | _ => skipWhite i state.input state.len
              }
            };
            switch items {
              | [Lexify p, ...rest] => loop i [p, ...rest]
              | [Empty, ...rest] => loop i rest
              | [Lookahead p, ...rest] => {
                let (i', _) = loop i [p];
                if (i' >= i) {
                  loop i rest
                } else {
                  (-1, [])
                }
              }
              | [Group g, ...rest] => loop i (List.concat [g, rest])
              | [Not p, ...rest] => {
                let (i', _) = loop i [p];
                if (i' >= i) {
                  (-1, [])
                } else {
                  loop i rest
                }
              }
              | [NonTerminal n label, ...rest] => {
                  let (i', result) = apply_rule grammar state n i;
                  if (i' >= i) {
                    let (i'', children) = loop i' rest;
                    (i'', [{...result, label}, ...children])
                  } else {
                    (-1, [])
                  }
                }
              | [Terminal str label, ...rest] => {
                  let slen = String.length str;
                  if (i + slen > state.len) {
                    (-1, [])
                  } else {
                    let sub = String.sub state.input i slen;
                    if (sub == str) {
                      let (i'', children) = loop (i + slen) rest;
                      (i'', [{label, start: i, cend: i + slen, children: [], typ: Terminal sub}, ...children])
                    } else {
                      (-1, [])
                    }
                  }
                }
              | [Any label, ...rest] =>
                if (i >= state.len) {
                  (-1, [])
                } else {
                  let (i'', children) = loop (i + 1) rest;
                  (i'', [{label, start: i, cend: i + 1, children: [], typ: Terminal (String.sub state.input i 1)}, ...children])
                }
              | [EOF, ...rest] => {
                if (i >= state.len) {
                  (i, [{label: None, start: i, cend: i, children: [], typ: Terminal ""}])
                } else {
                  (-1, [])
                }
              }
              | [Chars c1 c2 label, ...rest] =>
                if (i >= state.len) {
                  (-1, [])
                } else if (state.input.[i] >= c1 && state.input.[i] <= c2) {
                  let (i'', children) = loop (i + 1) rest;
                  (i'', [{label, start: i, cend: i + 1, children: [], typ: Terminal (String.sub state.input i 1)}, ...children])
                } else {
                  (-1, [])
                }
              | [Star subr label, ...rest] => {
                  let (i', subchildren) = greedy 0 None subr i;
                  if (i' >= i) {
                    let (i'', children) = loop i' rest;
                    (i'', [{label, start: i, cend: i', children: subchildren, typ: Iter}, ...children])
                  } else {
                    (-1, [])
                  }
                }
              | [Plus subr label, ...rest] => {
                  let (i', subchildren) = greedy 1 None subr i;
                  if (i' >= i) {
                    let (i'', children) = loop i' rest;
                    (i'', [{label, start: i, cend: i', children: subchildren, typ: Iter}, ...children])
                  } else {
                    (-1, [])
                  }
                }
              | [Optional subr label, ...rest] => {
                  let (i', subchildren) = greedy 0 (Some 1) subr i;
                  if (i' >= i) {
                    let (i'', children) = loop i' rest;
                    (i'', [{label, start: i, cend: i', children: subchildren, typ: Iter}, ...children])
                  } else {
                    (-1, [])
                  }
                }
              | [] => (i, [])
            }
          }

          and greedy min max subr i => {
            /* implements e* e+ e? */
            switch max {
            | Some 0 => (i, [])
            | _ =>
              if (min > 0) {
                /* we must match at least min or fail */
                let (i', found) = loop i [subr];
                if (i' >= i) {
                  let (i'', children) = greedy (min - 1) max subr i';
                  (i'', List.concat [found, children])
                } else {
                  (-1, [])
                }
              } else {
                /* try matching, doesn't matter if we fail */
                let (i', children) = loop i [subr];
                if (i' >= i) {
                  let max =
                    switch max {
                    | None => None
                    | Some n => Some (n - 1)
                    };
                  let (i'', more) = greedy 0 max subr i';
                  (i'', List.concat [children, more])
                } else {
                  (i, []) /* don't fail, return longest match */
                }
              }
            }
          };
          let (i', children) = loop i rs;
          if (i' >= i) {
            /* Printf.printf "match %s \"%s\" [%d..%d]\n" rulename name i (i' - 1); */
            let name = if ((List.length choices) === 1) { rulename } else {(rulename ^ "_" ^ sub_name)};
            let typ = isLexical ? (Lexical name (String.sub state.input i (i' - i))) : Nonlexical name;
            raise (Found (i', {start: i, cend: i', children, label: None, typ}))
          }
        }
      )
      choices;
    (-1, emptyState i rulename isLexical)
  } {
  | Found ans => ans
  }
};

let initialState input => {
  lrstack: [],
  cpos: 0,
  memo: Hashtbl.create 13,
  heads: Hashtbl.create 13,
  len: String.length input,
  input,
};

let parse grammar start input => {
  let state = initialState input;
  let (i, result) = apply_rule grammar state start 0;
  if (i == -1) {
    Failure None 0
  } else if (i < state.len) {
    Failure (Some result) i
  } else {
    Success result
  }
};
