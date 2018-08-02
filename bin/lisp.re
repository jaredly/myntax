
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

switch (Sysop.argv) {
  | [|_, "docs"|] =>
    let grammar = LispGrammar.grammar;
    open PackTypes.Parsing;
    let tbl = Hashtbl.create(10);
    List.iter(((name, rule)) => {
      let (sub, comment, items) = List.hd(rule.choices);
      if (List.length(rule.choices) > 1) {
        Printf.printf("\n\n### %s\n\n", name);
        print_endline("Name | Syntax\n--- | ---");
        rule.choices |> List.iter(((sub, comment, items)) => {
          print_endline(sub ++ " | " ++ "<code>" ++ ExampleGenerator.showSimple(ExampleGenerator.simpleForChoice(grammar, items), name) ++ "</code>")
        });
      } else if (comment != "") {
        print_endline("\n\n### " ++ name ++ "\n\n" ++ comment ++ "\n");
        print_endline("<code>" ++ ExampleGenerator.showSimple(ExampleGenerator.simpleForChoice(grammar, items), name) ++ "</code>");
      }
    }, grammar.rules |> List.rev);
    exit(0)
  | _ => ()
};

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


print_newline();