
let (grammarFile, input) = switch Sysop.argv {
  | [|_, grammarFile, input|] =>  (grammarFile, input)
  | _ => failwith "Usage: run grammarfile inputfile"
};

let contents = switch input {
  | "-" => Sysop.readStdin ()
  | x => Sysop.readFile x
};

let grammarRaw = Sysop.readFile grammarFile;

let errorText (isNot, rule) => {
  switch rule {
    | PackTypes.Parsing.Terminal text label => "Expected " ^ text
    | PackTypes.Parsing.Chars start cend label => Printf.sprintf "Expected %c..%c" start cend
    | _ => "Unknown problem"
  }
};

let lastLineLength txt pos => {
  try {
    let atNewline = String.get txt pos == '\n';
    let mpos = atNewline ? pos - 1 : pos;
    let lastPos = String.rindex_from txt mpos '\n';
    pos - lastPos - 1
  } {
    | Not_found => pos
  }
};

let leftPad base num coll => {
  let res = ref "";
  for i in 0 to num {
    res := base ^ !res;
  };
  !res
};

let showErrors errors text pos => {
  let showText = String.sub text 0 (String.index_from text pos '\n');
  Printf.eprintf "%s\n%s^\n" showText (leftPad "-" (lastLineLength text pos) "");
  (List.iter (fun err => {
    Printf.eprintf "%s\n" (errorText err)
  }) errors);
};

let start = Unix.gettimeofday();
let grammar = switch (Runtime.parse GrammarGrammar.grammar "Start" grammarRaw) {
  | PackTypes.Result.Failure maybeResult (charsParsed, (epos, errs)) => {
    Printf.eprintf "Incomplete parse of input file %d of %d total chars (%d)\n" charsParsed (String.length contents) epos;
    showErrors errs grammarRaw epos;
    failwith "Unable to parse grammar"
  }
  | PackTypes.Result.Success result => {
    let mid = Unix.gettimeofday();
    let res = GrammarOfGrammar.convert result;
    Printf.printf "Parse time %f, convert time %f\n" (mid -. start) (Unix.gettimeofday() -. mid);
    res
  }
};

switch (Runtime.parse grammar "Start" contents) {
  | PackTypes.Result.Failure maybeResult (charsParsed, (epos, errs)) => {
    /* switch maybeResult {
      | Some result => Json.result_to_string result |> print_endline
      | None => ()
    }; */
    showErrors errs contents epos;
    Printf.eprintf "Incomplete parse of input file %d of %d total chars (%d)\n" charsParsed (String.length contents) epos;
    exit 1;
  }
  | PackTypes.Result.Success result => {
    print_endline "Good";
    /* Json.result_to_string result |> print_endline; */
  }
};
