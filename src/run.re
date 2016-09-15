
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
  | PackTypes.Result.Failure maybeResult (charsParsed, failure) => {
    print_string (PackTypes.Result.genErrorText grammarRaw failure);
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
  | PackTypes.Result.Failure maybeResult (charsParsed, failure) => {
    /* switch maybeResult {
      | Some result => Json.result_to_string result |> print_endline
      | None => ()
    }; */
    print_string (PackTypes.Result.genErrorText contents failure);
    exit 1;
  }
  | PackTypes.Result.Success result => {
    print_endline "Good";
    /* Json.result_to_string result |> print_endline; */
  }
};
