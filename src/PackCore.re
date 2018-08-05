

/* Parser.
 * Packrat parser with left recursion, see:
 * "Packrat Parsers Can Support Left Recursion"
 * Alessandro Warth, James R. Douglass, Todd Millstein
 */
module StringSet = Set.Make(String);

type lr('result, 'errors) = {
  mutable seed: ans('result, 'errors),
  mutable rulename: string,
  mutable head: option(head)
}
and memoentry('result, 'errors) = {
  mutable ans: ans_or_lr('result, 'errors),
  mutable pos: int
}
and ans_or_lr('result, 'errors) =
  | Answer(ans('result, 'errors))
  | LR(lr('result, 'errors))
and ans('result, 'errors) = (int, 'result, 'errors)
and head = {
  mutable hrule: string,
  mutable involved_set: StringSet.t,
  mutable eval_set: StringSet.t
};

module T = {
  type state('result, 'errors) = {
    mutable lrstack: list(lr('result, 'errors)),
    memo: Hashtbl.t((StringSet.elt, int), memoentry('result, 'errors)),
    heads: Hashtbl.t(int, head),
    mutable cpos: int,
    len: int,
    input: string
  };
};
open T;

let initialState = (input) => {
  lrstack: [],
  cpos: 0,
  memo: Hashtbl.create(100),
  heads: Hashtbl.create(100),
  len: String.length(input),
  input
};

let tfst = ((a, _, _)) => a;

let unwrap = (opt) =>
  switch opt {
  | Some(x) => x
  | None => failwith("Expected Some(x)")
  };

let show_ans = (message, (pos, _, (epos, errors))) =>
  Printf.eprintf("%s :: (%d)\n%s\n", message, epos, PackTypes.Error.errorsText(errors));

let show_ansorlr = (message, ansor) =>
  switch ansor {
  | Answer(ans) => show_ans(message, ans)
  | LR(lr) => show_ans(message ++ "[lr seed]", lr.seed)
  };


type env('result, 'errors) = {
  state: state('result, 'errors),
  emptyResult: (int, string, bool) => 'result,
  mergeErrors: ('errors, 'errors) => 'errors,
};

/* Apply rule 'rulename' at position 'i' in the input.  Returns the new
 * position if the rule can be applied, else -1 if fails.
 */
let rec apply_rule = (~env, ~parse, rulename, i, ignoringNewlines, isNegated, path) => {
  /* print_endline ("Apply rule" ^ rulename); */
  let isLexical = Char.uppercase(rulename.[0]) != rulename.[0];
  switch (recall(~env, parse, rulename, i, isLexical, ignoringNewlines, isNegated, path)) {
  | None =>
    /* Printf.eprintf "New rule/pos %s %d\n" rulename i; */
    let lr = {seed: ((-1), env.emptyResult(i, rulename, isLexical), ((-1), [])), rulename, head: None};
    env.state.lrstack = [lr, ...env.state.lrstack];
    let memoentry = {ans: LR(lr), pos: i};
    Hashtbl.add(env.state.memo, (rulename, i), memoentry);
    let answer = parse(env.state, rulename, i, isLexical, ignoringNewlines, isNegated, path);
    env.state.lrstack = List.tl(env.state.lrstack);
    memoentry.pos = env.state.cpos;
    if (lr.head != None) {
      /* show_ans ("Replacing seed <" ^ rulename ^ "> " ^ (string_of_int i) ^ ": ") lr.seed; */
      /* show_ans (">> with ") answer; */
      lr.seed = answer;
      lr_answer(
        ~env,
        parse,
        rulename,
        i,
        memoentry,
        isLexical,
        ignoringNewlines,
        isNegated,
        path
      )
    } else {
      /* show_ansorlr ("Replacing memo <" ^ rulename ^ "> " ^ (string_of_int i) ^ ": ") memoentry.ans; */
      memoentry.ans = Answer(answer);
      answer
    }
  | Some(memoentry) =>
    /* Printf.eprintf "Old rule/pos %s %d\n" rulename i; */
    env.state.cpos = memoentry.pos;
    switch memoentry.ans {
    | LR(lr) =>
      setup_lr(~env, rulename, lr);
      lr.seed
    | Answer(answer) => answer
    }
  }
}
and setup_lr = (~env, rulename, lr) => {
  if (lr.head == None) {
    lr.head = Some({hrule: rulename, involved_set: StringSet.empty, eval_set: StringSet.empty})
  };
  let lr_head = unwrap(lr.head);
  let rec loop =
    fun
    | [] =>
      /* Printf.eprintf "If this ever happens it's probably OK to just remove this assert... see the old binop brach"; */
      /* assert false */
      ()
    | [l, ..._] when l.head == Some(lr_head) => ()
    | [l, ...ls] => {
        l.head = Some(lr_head);
        lr_head.involved_set = StringSet.add(l.rulename, lr_head.involved_set);
        loop(ls)
      };
  loop(env.state.lrstack)
}
and lr_answer =
    (~env, parse, rulename, i, memoentry, isLexical, ignoringNewlines, isNegated, path) => {
  let lr =
    switch memoentry.ans {
    | Answer(_) => assert false
    | LR(lr) => lr
    };
  let head =
    switch lr.head {
    | None => assert false
    | Some(head) => head
    };
  if (head.hrule != rulename) {
    lr.seed
  } else {
    memoentry.ans = Answer(lr.seed);
    if (tfst(lr.seed) == (-1)) {
      lr.
        seed
        /* (-1, emptyResult i rulename isLexical, (-1, [])) */
    } else {
      grow_lr(
        ~env,
        parse,
        rulename,
        i,
        memoentry,
        head,
        isLexical,
        ignoringNewlines,
        isNegated,
        path
      )
    }
  }
}
and recall = (~env, parse, rulename, i, isLexical, ignoringNewlines, isNegated, path) => {
  let maybeEntry =
    try (Some(Hashtbl.find(env.state.memo, (rulename, i)))) {
    | Not_found => None
    };
  let maybeHead =
    try (Some(Hashtbl.find(env.state.heads, i))) {
    | Not_found => None
    };
  switch maybeHead {
  | None => maybeEntry
  | Some(head) =>
    if (maybeEntry == None
        && ! StringSet.mem(rulename, StringSet.add(head.hrule, head.involved_set))) {
      Some({ans: Answer(((-1), env.emptyResult(i, rulename, isLexical), ((-1), []))), pos: i})
    } else {
      if (StringSet.mem(rulename, head.eval_set)) {
        head.eval_set = StringSet.remove(rulename, head.eval_set);
        let answer =
          parse(env.state, rulename, i, isLexical, ignoringNewlines, isNegated, path);
        /* Original paper RECALL function seems to have a bug ... */
        let memoentry = unwrap(maybeEntry);
        memoentry.ans = Answer(answer);
        memoentry.pos = env.state.cpos
      };
      maybeEntry
    }
  }
}
and grow_lr =
    (~env, parse, rulename, i, memoentry, head, isLexical, ignoringNewlines, isNegated, path) => {
  Hashtbl.replace(env.state.heads, i, head); /* A */
  let rec loop = () => {
    env.state.cpos = i;
    head.eval_set = head.involved_set; /* B */
    let ans = parse(env.state, rulename, i, isLexical, ignoringNewlines, isNegated, path);

    /*** NOTE(jared): I added oans & the check b/c without it some left recursion still wasn't working */
    let oans =
      switch memoentry.ans {
      | Answer((i, _, _)) => i
      | LR(_) => (-1)
      };
    if (tfst(ans) == (-1) || env.state.cpos <= memoentry.pos && tfst(ans) <= oans) {
      /*** Merge errors with those of previous answer **/
      switch memoentry.ans {
      | LR(_) => ()
      | Answer((a, b, oerrs)) =>
        let (_, _, aerrs) = ans;
        memoentry.ans = Answer((a, b, env.mergeErrors(oerrs, aerrs)))
      };
      ()
    } else {
      memoentry.ans = Answer(ans);
      memoentry.pos = env.state.cpos;
      loop()
    }
  };
  loop();
  Hashtbl.remove(env.state.heads, i); /* C */
  env.state.cpos = memoentry.pos;
  switch memoentry.ans {
  | Answer(answer) =>
    let (_, _, (epos, _)) = answer;
    /* Printf.eprintf "grow_lr < %s(%d) : %d\n" rulename i epos; */
    answer
  | LR(_) => assert false
  }
};