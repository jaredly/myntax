
let (grammar, input) = switch Sysop.argv {
  | [|_, grammar, input|] =>  (grammar, input)
  | _ => failwith "Usage: run grammarfile inputfile"
};

let contents = switch input {
  | "-" => Sysop.readStdin ()
  | x => Sysop.readFile x
};

let grammarRaw = Sysop.readFile grammar;

let grammar = switch (Runtime.parse GrammarGrammar.grammar "Start" grammarRaw) {
  | Runtime.Failed message => failwith "Unable to parse grammar"
  | Runtime.Incomplete _ => failwith "Grammar only partially parsed"
  | Runtime.Complete result => GrammarOfGrammar.convert result
};

switch (Runtime.parse grammar "Start" contents) {
  | Runtime.Failed message => failwith "Unable to parse input file"
  | Runtime.Incomplete (i, result) => {
    Json.result_to_string result |> print_endline;
    Printf.eprintf "Incomplete parse of input file %d of %d total chars" i (String.length contents);
    exit 1;
  }
  | Runtime.Complete result => {
    Json.result_to_string result |> print_endline;
  }
};
