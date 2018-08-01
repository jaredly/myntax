


let choice = raw => {
  switch (Runtime.parse(GrammarGrammar.grammar, "Choice", raw)) {
  | PackTypes.Result.Failure(maybeResult, (charsParsed, failure)) =>
    Printf.eprintf("%s\n", PackTypes.Error.genErrorText(raw, failure));
    failwith("Unable to parse grammar")
  | PackTypes.Result.Success(Node(_, children, _)) =>
    let mid = Unix.gettimeofday();
    let (_, _, res) = GrammarOfGrammar.parseChoice(children);
    res
  | PackTypes.Result.Success(Leaf(_)) => assert(false)
  }
};