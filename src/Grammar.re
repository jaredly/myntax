
let choice = raw => {
  switch (Runtime.parse(GrammarGrammar.grammar, "Choice", raw)) {
  | Error((maybeResult, (charsParsed, failure))) =>
    Printf.eprintf("%s\n", PackTypes.Error.genErrorText(raw, failure));
    failwith("Unable to parse grammar")
  | Belt.Result.Ok(Node(_, children, _)) =>
    let mid = Unix.gettimeofday();
    let (_, _, res) = GrammarOfGrammar.parseChoice(children);
    res
  | Belt.Result.Ok(Leaf(_)) => assert(false)
  }
};

let getResult = (grammar, entry, contents) => {
  switch (Runtime.parse(grammar, entry, contents)) {
  | Belt.Result.Error((maybeResult, (charsParsed, failure))) =>
    Printf.eprintf("%s\n", PackTypes.Error.genErrorText(contents, failure));
    exit(10)
  | Belt.Result.Ok(Node((_, sub), children, loc)) => {
    /* print_endline(PackTypes.Result.showLoc(loc)); */
    (sub, children, loc)
  }
  | Belt.Result.Ok(Leaf(_)) => failwith("parse should not be a leaf")
  }
};