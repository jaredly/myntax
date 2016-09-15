type printType = Bin | Pretty | Debug;
let (grammarFile, input, printType) = switch Sysop.argv {
  | [|_, "bin", grammarFile, input|] =>  (grammarFile, input, Bin)
  | [|_, "pretty", grammarFile, input|] =>  (grammarFile, input, Pretty)
  | [|_, grammarFile, input|] =>  (grammarFile, input, Debug)
  | _ => failwith "Usage: run grammarfile inputfile"
};

let contents = switch input {
  | "-" => Sysop.readStdin ()
  | x => Sysop.readFile x
};

let grammarRaw = Sysop.readFile grammarFile;

let printImpl implementation => {
  Printast.implementation Format.std_formatter implementation;
};

let start = Unix.gettimeofday();
let grammar = switch (Runtime.parse GrammarGrammar.grammar "Start" grammarRaw) {
  | PackTypes.Result.Failure maybeResult (charsParsed, failure) => {
    print_string (PackTypes.Result.genErrorText grammarRaw failure);
    failwith "Unable to parse grammar"
  }
  | PackTypes.Result.Success result => {
    let mid = Unix.gettimeofday();
    let res = GrammarOfGrammar.convert result;
    Printf.eprintf "Parse time %f, convert time %f\n" (mid -. start) (Unix.gettimeofday() -. mid);
    res
  }
};

let out_binary ast => {
  output_string stdout Config.ast_impl_magic_number;
  output_value stdout input;
  output_value stdout ast
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
    /* print_endline "Good"; */
    switch printType {
      | Bin => out_binary (OcamlOfReason.convert result);
      | Debug => printImpl (OcamlOfReason.convert result);
      | Pretty => print_endline (PrettyPrint.toString grammar result);
    }
    /* Json.result_to_string result |> print_endline; */
  }
};
