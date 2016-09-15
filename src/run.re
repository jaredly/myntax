
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
  | PackTypes.Result.Failure maybeResult partial => {
    failwith "Unable to parse grammar"
  }
  | PackTypes.Result.Success result => {
    let mid = Unix.gettimeofday();
    let res = GrammarOfGrammar.convert result;
    Printf.printf "Parse time %f, convert time %f" (mid -. start) (Unix.gettimeofday() -. mid);
    res
  }
};

switch (Runtime.parse grammar "Start" contents) {
  | PackTypes.Result.Failure maybeResult partial => {
    switch maybeResult {
      | Some result => Json.result_to_string result |> print_endline
      | None => ()
    };
    Printf.eprintf "Incomplete parse of input file %d of %d total chars" partial (String.length contents);
    exit 1;
  }
  | PackTypes.Result.Success result => {
    Json.result_to_string result |> print_endline;
  }
};
