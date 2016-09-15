
let readFile chan => {
  let lines = ref [];
  try (
    while true {
      lines := [input_line chan, ...!lines]
    }
  ) {
  | End_of_file => ()
  };
  let lines = List.rev !lines;
  String.concat "\n" lines
};

let (grammar, input) = switch Sys.argv {
  | [|_, grammar, input|] =>  (grammar, input)
  | _ => failwith "Usage: run grammarfile inputfile"
};

let contents = readFile (switch input {
  | "-" => stdin
  | x => open_in x
});

let grammarRaw = open_in grammar |> readFile;

let grammar = switch (Runtime.parse GrammarGrammar.grammar "Start" grammarRaw) {
  | Runtime.Failed message => failwith "Unable to parse grammar"
  | Runtime.Incomplete _ => failwith "Grammar only partially parsed"
  | Runtime.Complete result => GrammarOfGrammar.convert result
};

/* print_endline(PackTypes.Parsing.show_grammar grammar); */

switch (Runtime.parse grammar "Start" contents) {
  | Runtime.Failed message => failwith "Unable to parse input file"
  | Runtime.Incomplete (i, result) => {
    PackTypes.Result.result_to_yojson result |> Yojson.Safe.to_string |> print_endline;
    failwith "Incomplete parse of input file"
  }
  | Runtime.Complete result => {
    PackTypes.Result.result_to_yojson result |> Yojson.Safe.to_string |> print_endline;
  }
}
