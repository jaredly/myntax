/* from https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/ with modifications */
open PackTypes;
let initial_state = "Start";
let grammar = SimpleGrammar.grammar;

/* To support backtracking, need to read all input. */
let input = {
  let lines = ref [];
  try (
    while true {
      lines := [read_line (), ...!lines]
    }
  ) {
  | End_of_file => ()
  };
  let lines = List.rev !lines;
  String.concat " " lines
};

let len = String.length input;

/* Parser.
 * Packrat parser with left recursion, see:
 * "Packrat Parsers Can Support Left Recursion"
 * Alessandro Warth, James R. Douglass, Todd Millstein
 */
let module StringSet = Set.Make String;

/* type prodState = {
  name: string,
  choice: string,
  children: list parseState
}
and parseState = {
  label: string,
  typ: string,
  children: list parseState
}; */

type stateType =
  | Terminal
  | Lexical
  | Iter
  | Nonlexical [@@deriving yojson];

type state = {
  start: int,
  cend: int,
  typ: stateType,
  name: option string,
  children: list state,
} [@@deriving yojson];

/* type stateType =
  | Production
  | Choice
  | Star
  | Plus
  | Optional;
type state = {
  productionName: string,
  children: list state,
}; */

type lr = {mutable seed: ans, mutable rulename: string, mutable head: option head}
and memoentry = {mutable ans: ans_or_lr, mutable pos: int}
and ans_or_lr = | Answer ans | LR lr
and ans = (int, state)
and head = {
  mutable hrule: string,
  mutable involved_set: StringSet.t,
  mutable eval_set: StringSet.t
};

exception Found ans;

let emptyState pos name typ => {start: pos, cend: pos, typ, name: Some name, children: []};

let lrstack = ref [];
let memo = Hashtbl.create 13;
let pos = ref 0;
let heads = Hashtbl.create 13;

let optionGet v =>
  switch v {
    | Some x => x
    | None => failwith "Expected Some(x)"
  };

let fst (a, _) => a;

/* Apply rule 'rulename' at position 'i' in the input.  Returns the new
 * position if the rule can be applied, else -1 if fails.
 */
let rec apply_rule rulename i => {
  let typ = if ((Char.uppercase (String.get rulename 0)) == (String.get rulename 0)) {
    Nonlexical
  } else {
    Lexical
  };
  switch (recall rulename i typ) {
  | None =>
    let lr = {seed: (-1, emptyState i rulename typ), rulename, head: None};
    lrstack := [lr, ...!lrstack];
    let m = {ans: LR lr, pos: i};
    Hashtbl.add memo (rulename, i) m;
    let r = parse rulename i typ;
    lrstack := List.tl !lrstack;
    m.pos = !pos;
    if (lr.head != None) {
      lr.seed = r;
      lr_answer rulename i m typ
    } else {
      m.ans = Answer r;
      r
    }
  | Some m =>
    pos := m.pos;
    switch m.ans {
    | LR lr =>
      setup_lr rulename lr;
      lr.seed
    | Answer r => r
    }
  }
}

and setup_lr rulename lr => {
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
  loop !lrstack
}

and lr_answer rulename i m typ => {
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
      (-1, emptyState i rulename typ)
    } else {
      grow_lr rulename i m h typ
    }
  }
}

and recall rulename i typ => {
  let m =
    try (Some (Hashtbl.find memo (rulename, i))) {
    | Not_found => None
    };
  let h =
    try (Some (Hashtbl.find heads i)) {
    | Not_found => None
    };
  switch h {
  | None => m
  | Some h =>
    if (m == None && not (StringSet.mem rulename (StringSet.add h.hrule h.involved_set))) {
      Some {ans: Answer (-1, emptyState i rulename Nonlexical), pos: i}
    } else {
      if (StringSet.mem rulename h.eval_set) {
        h.eval_set = StringSet.remove rulename h.eval_set;
        let r = parse rulename i typ;
        /* Original paper RECALL function seems to have a bug ... */
        let m = optionGet m;
        m.ans = Answer r;
        m.pos = !pos
      };
      m
    }
  }
}

and grow_lr rulename i m h typ => {
  Hashtbl.replace heads i h; /* A */
  let rec loop () => {
    pos := i;
    h.eval_set = h.involved_set; /* B */
    let ans = parse rulename i typ;
    if (fst ans == (-1) || !pos <= m.pos) {
      ()
    } else {
      m.ans = Answer ans;
      m.pos = !pos;
      loop ()
    }
  };
  loop ();
  Hashtbl.remove heads i; /* C */
  pos := m.pos;
  switch m.ans {
  | Answer r => r
  | LR _ => assert false
  }
}

and parse rulename i typ => {
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
          let rec loop i =>
            fun
            | [Empty, ...rest] => loop i rest
            | [Lookahead p, ...rest] => {
              let (i', _) = loop i [p];
              if (i' >= i) {
                loop i rest
              } else {
                (-1, [])
              }
            }
            | [Not p, ...rest] => {
              let (i', _) = loop i [p];
              if (i' >= i) {
                (-1, [])
              } else {
                loop i rest
              }
            }
            | [NonTerminal n name, ...rest] => {
                let (i', state) = apply_rule n i;
                if (i' >= i) {
                  let (i'', children) = loop i' rest;
                  (i'', [(
                    switch name {
                      | Some x => {name, start: i, cend: i', children: [state], typ: Nonlexical}
                      | None => state
                    }
                  ), ...children])
                } else {
                  (-1, [])
                }
              }
            | [Terminal str name, ...rest] => {
                let slen = String.length str;
                if (i + slen > len) {
                  (-1, [])
                } else {
                  let sub = String.sub input i slen;
                  if (sub == str) {
                    let (i'', children) = loop (i + slen) rest;
                    (i'', [{name, start: i, cend: i + slen, children: [], typ: Terminal}, ...children])
                  } else {
                    (-1, [])
                  }
                }
              }
            | [Chars c1 c2 name, ...rest] =>
              if (i >= len) {
                (-1, [])
              } else if (input.[i] >= c1 && input.[i] <= c2) {
                let (i'', children) = loop (i + 1) rest;
                (i'', [{name, start: i, cend: i + 1, children: [], typ: Terminal}, ...children])
              } else {
                (-1, [])
              }
            | [Star subr name, ...rest] => {
                let (i', subchildren) = greedy 0 None subr i;
                if (i' >= i) {
                  let (i'', children) = loop i' rest;
                  (i'', [{name, start: i, cend: i', children: subchildren, typ: Iter}, ...children])
                } else {
                  (-1, [])
                }
              }
            | [Plus subr name, ...rest] => {
                let (i', subchildren) = greedy 1 None subr i;
                if (i' >= i) {
                  let (i'', children) = loop i' rest;
                  (i'', [{name, start: i, cend: i', children: subchildren, typ: Iter}, ...children])
                } else {
                  (-1, [])
                }
              }
            | [Optional subr name, ...rest] => {
                let (i', subchildren) = greedy 0 (Some 1) subr i;
                if (i' >= i) {
                  let (i'', children) = loop i' rest;
                  (i'', [{name, start: i, cend: i', children: subchildren, typ: Iter}, ...children])
                } else {
                  (-1, [])
                }
              }
            | [] => (i, [])
          and greedy min max subr i => {
            /* implements e* e+ e? */
            /* Printf.printf
              "greedy %d %s %d\n"
              min
              (
                switch max {
                | None => "None"
                | Some n => Printf.sprintf "Some %d" n
                }
              )
              i; */
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
            raise (Found (i', {start: i, cend: i', children, name: Some name, typ}))
          }
        }
      )
      choices;
    (-1, emptyState i rulename typ)
  } {
  | Found ans => ans
  }
};

let () = {
  let (i, state) = apply_rule initial_state 0;
  if (i == (-1)) {
    /* TODO: report errors! */
    Printf.eprintf "parse error: parsing failed\n";
    exit 1
  } else if (i < len) {
    Printf.eprintf "parse error: extra characters after end of input\n";
    exit 1
  } else {
    Printf.printf "%s" (Yojson.Safe.to_string(state_to_yojson state));
    /* Printf.printf "parsed OK\n" */
  }
};
