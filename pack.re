/* https://rwmj.wordpress.com/2010/12/29/packrat-parser-with-left-recursion/ */
type grammar = list production
and production = (string, list choice) /* rule name -> e1 | e2 | ... */
and choice = (string, string, list parsing) /* choice name, comment, sequence */
and parsing =
  | Star parsing       /* e* */
  | Plus parsing       /* e+ */
  | Optional parsing   /* e? */
  | Lookahead parsing  /* &e */
  | Not parsing        /* !e */
  | NonTerminal string /* nonterminal 'name' */
  | Terminal string    /* terminal */
  | Chars char char    /* [a-z] */
  | Empty              /* epsilon */;

let grammar: grammar = [
  /*
     start -> expr
   */
  ("start", [("", "", [NonTerminal "expr"])]),
  /*
     expr ->
           | expr expr
           | "fun" patt "->" expr = "fundecl" ; function
           | "(" expr ")"
           | expr "+" expr
           | ident
           | int64
   */
  (
    "expr",
    [
      ("funappl", "", [NonTerminal "expr", NonTerminal "sp", NonTerminal "expr"]),
      (
        "fundecl",
        "function declaration",
        [Terminal "fun", NonTerminal "sp", NonTerminal "patt", NonTerminal "osp", Terminal "->", NonTerminal "sp", NonTerminal "expr"]
      ),
      ("paren", "", [Terminal "(", NonTerminal "osp", NonTerminal "expr", NonTerminal "osp", Terminal ")"]),
      ("addition", "", [NonTerminal "expr", NonTerminal "osp", Terminal "+", NonTerminal "osp", NonTerminal "expr"]),
      ("ident", "", [NonTerminal "ident"]),
      ("int64", "", [NonTerminal "int64"])
    ]
  ),
  /*
     patt -> ident
   */
  ("patt", [("", "", [NonTerminal "ident"])]),
  /*
     int64 -> digit19 digit*
            | digit
     digit -> [0-9]
     digit19 -> [1-9]
   */
  (
    "int64",
    [
      /* ("", "", [NonTerminal "digit19", Star (NonTerminal "digit")]) */
      ("", "", [Not (Terminal "0"), Plus (NonTerminal "digit")])
      /* ("", "", [Plus(NonTerminal "digit")]) */
    ]
  ),
  ("digit", [("", "", [Chars '0' '9'])]),
  ("digit19", [("", "", [Chars '1' '9'])]),
  /*
     ident -> alpha+
     alpha -> [a-z]
   */
  ("ident", [("", "", [Not (NonTerminal "reserved"), Plus (NonTerminal "alpha")])]),
  ("reserved", [
    ("", "", [Terminal "let"]),
    ("", "", [Terminal "for"]),
    ("", "", [Terminal "fun"]),
  ]),
  ("alpha", [("", "", [Chars 'a' 'z'])]),
  /*
     sp -> " "+
     osp -> " "*
   */
  ("sp", [("", "", [Plus (Terminal " ")])]),
  ("osp", [("", "", [Star (Terminal " ")])])
];

let initial_state = "start";

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

exception Found int;

type lr = {mutable seed: ans, mutable rulename: string, mutable head: option head}
and memoentry = {mutable ans: ans_or_lr, mutable pos: int}
and ans_or_lr = | Answer ans | LR lr
and ans = int
and head = {
  mutable hrule: string,
  mutable involved_set: StringSet.t,
  mutable eval_set: StringSet.t
};

let lrstack = ref [];
let memo = Hashtbl.create 13;
let pos = ref 0;
let heads = Hashtbl.create 13;

let optionGet v =>
  switch v {
    | Some x => x
    | None => failwith "Expected Some(x)"
  };

/* Apply rule 'rulename' at position 'i' in the input.  Returns the new
 * position if the rule can be applied, else -1 if fails.
 */
let rec apply_rule rulename i =>
  switch (recall rulename i) {
  | None =>
    let lr = {seed: (-1), rulename, head: None};
    lrstack := [lr, ...!lrstack];
    let m = {ans: LR lr, pos: i};
    Hashtbl.add memo (rulename, i) m;
    let r = parse rulename i;
    lrstack := List.tl !lrstack;
    m.pos = !pos;
    if (lr.head != None) {
      lr.seed = r;
      lr_answer rulename i m
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
and lr_answer rulename i m => {
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
    if (lr.seed == (-1)) {
      (-1)
    } else {
      grow_lr rulename i m h
    }
  }
}
and recall rulename i => {
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
      Some {ans: Answer (-1), pos: i}
    } else {
      if (StringSet.mem rulename h.eval_set) {
        h.eval_set = StringSet.remove rulename h.eval_set;
        let r = parse rulename i;
        /* Original paper RECALL function seems to have a bug ... */
        let m = optionGet m;
        m.ans = Answer r;
        m.pos = !pos
      };
      m
    }
  }
}
and grow_lr rulename i m h => {
  Hashtbl.replace heads i h; /* A */
  let rec loop () => {
    pos := i;
    h.eval_set = h.involved_set; /* B */
    let ans = parse rulename i;
    if (ans == (-1) || !pos <= m.pos) {
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

and parse rulename i => {
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
        fun (name, comment, rs) => {
          /* Printf.printf "parse %s \"%s\" %d\n" rulename name i; */
          let rec loop i =>
            fun
            | [Empty, ...rest] => loop i rest
            | [Lookahead p, ...rest] => {
              let i' = loop i [p];
              if (i' >= i) {
                loop i rest
              } else {
                -1
              }
            }
            | [Not p, ...rest] => {
              let i' = loop i [p];
              if (i' >= i) {
                -1
              } else {
                loop i rest
              }
            }
            | [NonTerminal n, ...rest] => {
                let i' = apply_rule n i;
                if (i' >= i) {
                  loop i' rest
                } else {
                  (-1)
                }
              }
            | [Terminal str, ...rest] => {
                let slen = String.length str;
                if (i + slen > len) {
                  (-1)
                } else {
                  let sub = String.sub input i slen;
                  if (sub == str) {
                    loop (i + slen) rest
                  } else {
                    (-1)
                  }
                }
              }
            | [Chars c1 c2 [@implicit_arity], ...rest] =>
              if (i >= len) {
                (-1)
              } else if (input.[i] >= c1 && input.[i] <= c2) {
                loop (i + 1) rest
              } else {
                (-1)
              }
            | [Star subr, ...rest] => {
                let i' = greedy 0 None subr i;
                if (i' >= i) {
                  loop i' rest
                } else {
                  (-1)
                }
              }
            | [Plus subr, ...rest] => {
                let i' = greedy 1 None subr i;
                if (i' >= i) {
                  loop i' rest
                } else {
                  (-1)
                }
              }
            | [Optional subr, ...rest] => {
                let i' = greedy 0 (Some 1) subr i;
                if (i' >= i) {
                  loop i' rest
                } else {
                  (-1)
                }
              }
            | [] => i
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
            | Some 0 => i
            | _ =>
              if (min > 0) {
                /* we must match at least min or fail */
                let i' = loop i [subr];
                if (i' >= i) {
                  greedy (min - 1) max subr i'
                } else {
                  (-1)
                }
              } else {
                /* try matching, doesn't matter if we fail */
                let i' = loop i [subr];
                if (i' >= i) {
                  let max =
                    switch max {
                    | None => None
                    | Some n => Some (n - 1)
                    };
                  greedy 0 max subr i'
                } else {
                  i /* don't fail, return longest match */
                }
              }
            }
          };
          let i' = loop i rs;
          if (i' >= i) {
            /* Printf.printf "match %s \"%s\" [%d..%d]\n" rulename name i (i' - 1); */
            raise (Found i')
          }
        }
      )
      choices;
    (-1)
  } {
  | Found i => i
  }
};

let () = {
  let i = apply_rule initial_state 0;
  if (i == (-1)) {
    Printf.eprintf "parse error: parsing failed\n";
    exit 1
  } else if (i < len) {
    Printf.eprintf "parse error: extra characters after end of input\n";
    exit 1
  } else {
    Printf.printf "parsed OK\n"
  }
};
