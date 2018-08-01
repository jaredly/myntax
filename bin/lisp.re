
let getContents = (input) =>
  switch input {
  | "-" => Sysop.readStdin()
  | x => Sysop.readFile(x)
  };

let out_binary = (ast: Parsetree.structure, input_name) => {
  output_string(stdout, Config.ast_impl_magic_number);
  output_value(stdout, input_name);
  output_value(stdout, ast)
};

let printImpl = (implementation) => Printast.implementation(Format.std_formatter, implementation);
let pprintImpl = (implementation) => Pprintast.structure(Format.std_formatter, implementation);

let getResult = (grammar, contents) => {
  let start = Unix.gettimeofday();
  switch (Runtime.parse(grammar, "Start", contents)) {
  | PackTypes.Result.Failure(maybeResult, (charsParsed, failure)) =>
    Printf.eprintf("%s\n", PackTypes.Error.genErrorText(contents, failure));
    exit(10)
  | PackTypes.Result.Success(Node((_, sub), children, loc)) => ((sub, children, loc), Unix.gettimeofday() -. start)
  | PackTypes.Result.Success(Leaf(_)) => assert(false)
  }
};

type printType =
  | Bin
  | Pretty(string)
  | Debug
  | Ml
  ;

let (command, grammarFile, input) =
  switch Sysop.argv {
  | [|_, "pretty", grammarFile, input, output|] => (Pretty(output), grammarFile, input)
  | [|_, command, grammarFile, input|] => (
      switch command {
      | "bin" => Bin
      | "ml" => Ml
      | "debug" => Debug
      | "pretty" => Pretty("-")
      | _ => failwith("Invalid command")
      },
      grammarFile,
      input
    )
  | [|_, grammarFile, input|] => (Debug, grammarFile, input)
  | _ => failwith("Usage: [command=debug] grammarfile inputfile")
  };

let (result, raw) = getResult(LispGrammar.grammar, input);

switch command {
| Debug => printImpl(LispGrammar.convert_Start(result))
| Ml => pprintImpl(LispGrammar.convert_Start(result))
| Bin => out_binary(LispGrammar.convert_Start(result), input)
| Pretty(dest) =>
  switch (PrettyPrint.startToString(LispGrammar.grammar, result)) {
  | Some(x) =>
    switch dest {
    | "-" => print_endline(x)
    | _ => output_string(open_out(dest), x)
    }
  | None => failwith("Failed to pretty print :(")
  }
};

