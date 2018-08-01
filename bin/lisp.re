
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

let getResult = (grammar, entry, contents) => {
  let start = Unix.gettimeofday();
  switch (Runtime.parse(grammar, entry, contents)) {
  | PackTypes.Result.Failure(maybeResult, (charsParsed, failure)) =>
    Printf.eprintf("%s\n", PackTypes.Error.genErrorText(contents, failure));
    exit(10)
  | PackTypes.Result.Success(Node((_, sub), children, loc)) => {
    /* print_endline(PackTypes.Result.showLoc(loc)); */
    ((sub, children, loc), Unix.gettimeofday() -. start)
  }
  | PackTypes.Result.Success(Leaf(_)) => failwith("parse should not be a leaf")
  }
};

Printexc.record_backtrace(true);

type printType =
  | Bin
  | Pretty(string)
  | Debug
  | Ml
  ;

let (command, input) =
  switch Sysop.argv {
  | [|_, "pretty", input, output|] => (Pretty(output), input)
  | [|_, command, input|] => (
      switch command {
      | "bin" => Bin
      | "ml" => Ml
      | "debug" => Debug
      | "pretty" => Pretty("-")
      | _ => failwith("Invalid command")
      },
      input
    )
  | [|_, input|] => (Debug, input)
  | _ => failwith("Usage: [command=debug] grammarfile inputfile")
  };

let (result, raw) = getResult(LispGrammar.grammar, "Start", getContents(input));
let converted = LispGrammar.convert_Start(result);
/* print_endline(string_of_int(List.length(converted))); */

/* Printast.implementation(Format.std_formatter, converted); */

switch command {
| Debug => Printast.implementation(Format.std_formatter, converted)
| Ml =>    Pprintast.structure(Format.std_formatter, converted)
| Bin =>   out_binary(converted, input)
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

