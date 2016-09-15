
let (grammarFile, input) = switch Sysop.argv {
  | [|_, grammarFile, input|] =>  (grammarFile, input)
  | _ => failwith "Usage: run grammarfile inputfile"
};

let contents = switch input {
  | "-" => Sysop.readStdin ()
  | x => Sysop.readFile x
};

let grammarRaw = Sysop.readFile grammarFile;

let start = Unix.gettimeofday();
let grammar = switch (Runtime.parse GrammarGrammar.grammar "Start" grammarRaw) {
  | PackTypes.Result.Failure maybeResult (charsParsed, (epos, errs)) => {
    Printf.eprintf "Incomplete parse of input file %d of %d total chars (%d)\n" charsParsed (String.length contents) epos;
    failwith "Unable to parse grammar"
  }
  | PackTypes.Result.Success result => {
    let mid = Unix.gettimeofday();
    let res = GrammarOfGrammar.convert result;
    Printf.printf "Parse time %f, convert time %f\n" (mid -. start) (Unix.gettimeofday() -. mid);
    res
  }
};

let errorText (isNot, rule) => {
  switch rule {
    | PackTypes.Parsing.Terminal text label => "Expected " ^ text
    | _ => "Unknown problem"
  }
};

switch (Runtime.parse grammar "Start" contents) {
  | PackTypes.Result.Failure maybeResult (charsParsed, (epos, errs)) => {
    /* switch maybeResult {
      | Some result => Json.result_to_string result |> print_endline
      | None => ()
    }; */
    (List.iter
      (fun err => {
        Printf.eprintf "%s\n" (errorText err)
      })
      errs);
    Printf.eprintf "%s\n" (String.sub contents 0 epos);
    Printf.eprintf "Incomplete parse of input file %d of %d total chars (%d)\n" charsParsed (String.length contents) epos;
    exit 1;
  }
  | PackTypes.Result.Success result => {
    print_endline "Good";
    /* Json.result_to_string result |> print_endline; */
  }
};
