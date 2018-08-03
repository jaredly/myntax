
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

Printexc.record_backtrace(true);

type printType =
  | Bin
  | Pretty(string)
  | Debug
  | Ml
  ;

switch (Sysop.argv) {
  | [|_, "docs"|] =>
    open PackTypes.Parsing;
    print_endline("# Syntax for Lisp.re\n");
    print_endline(ExampleGenerator.help ++ "\n\nIf you're interested, <a href=\"../grammars/lispGrammar.re\">take a look at the grammar definition</a>");
    print_endline(ExampleGenerator.docsForGrammar(LispGrammar.grammar));
    exit(0)
  | [|_, "docs-docs"|] =>
    open PackTypes.Parsing;
    print_endline("# Syntax for the Parser Generator\n");
    print_endline(ExampleGenerator.help ++ "\n\nIf you're interested, <a href=\"../parsable/grammar\">take a look at the grammar definition</a>");
    print_endline(ExampleGenerator.docsForGrammar(GrammarGrammar.grammar));
    exit(0)
  | [|_, "--print", "binary", "--parse", "re"|] =>
    let raw = Sysop.readStdin();
    let result = switch (LispGrammar.start("Start", raw)) {
      | x => x
      | exception PackTypes.ConversionError(loc, ruleName, searchedFor) =>
      failwith("Grammar error! Please report this to the lisp.re maintainers. Unable to find " ++ searchedFor ++ " in " ++ ruleName ++ " at " ++ PackTypes.Result.showLoc(loc))
    };
    switch result {
      | Ok(converted) => out_binary(converted, "inputfile.rel");
      | Error((_, (_, loc, failure))) =>
        Printf.eprintf("File \"%s\", line %d, characters %d-%d:\n%s",
          loc.pos_fname, loc.pos_lnum, loc.pos_cnum - loc.pos_bol, loc.pos_cnum - loc.pos_bol + 10,
          PackTypes.Error.errorsText(snd(failure))
        );
        exit(1)
    };
    exit(0)
  | [|_, "--print", "binary", "--recoverable", "--parse", "re"|] =>
    let raw = Sysop.readStdin();
    let result = switch (LispGrammar.start("Start", raw)) {
      | x => x
      | exception PackTypes.ConversionError(loc, ruleName, searchedFor) =>
        failwith("Grammar error! Please report this to the lisp.re maintainers. Unable to find " ++ searchedFor ++ " in " ++ ruleName ++ " at " ++ PackTypes.Result.showLoc(loc))
    };
    switch result {
      | Ok(converted) => out_binary(converted, "inputfile.rel");
      | Error((Some(converted), (_, loc, failure))) =>
          out_binary(converted, "inputfile.rel");
          Printf.eprintf("File \"%s\", line %d, characters %d-%d:\n%s",
            loc.pos_fname, loc.pos_lnum, loc.pos_cnum - loc.pos_bol, loc.pos_cnum - loc.pos_bol + 10,
            PackTypes.Error.errorsText(snd(failure))
          )
      | Error((None, (_, loc, failure))) =>
          out_binary([], "inputfile.rel");
          Printf.eprintf("File \"%s\", line %d, characters %d-%d:\n%s",
            loc.pos_fname, loc.pos_lnum, loc.pos_cnum - loc.pos_bol, loc.pos_cnum - loc.pos_bol + 10,
            PackTypes.Error.errorsText(snd(failure))
          )
    };
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

let result = Grammar.getResult(LispGrammar.grammar, "Start", getContents(input));
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
