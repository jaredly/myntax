
let rx = Str.regexp "PackTypes\\.Parsing\\.";
let replaceModule = Str.global_replace rx "";
let module P = PackTypes.Parsing;

let printGrammar grammar name => {
  Printf.sprintf {|(** This grammar definition was generated from %s **)
open PackTypes.Parsing

let grammar = %s;

|} name (replaceModule (P.show_grammar grammar));

};

let main dump::dump=false file::file=? dest::dest=? () => {
  let contents = switch file {
    | Some name => Sysop.readFile name
    | None => Sysop.readStdin()
  };
  switch (Runtime.parse GrammarGrammar.grammar "Start" contents) {
    | PackTypes.Result.Failure maybeResult (charsParsed, failure) => {
      Printf.eprintf "%s\n" (PackTypes.Error.genErrorText contents failure);
      exit 1;
    }
    | PackTypes.Result.Success result => {
      /* print_endline (PackTypes.show_result result); */
      let name = switch file {
        | Some name => name
        | None => "stdin"
      };
      if dump {
        let grammar = GrammarOfGrammar.convert result;
        switch dest {
          | Some dest => Printf.fprintf (open_out dest) "%s" (printGrammar grammar name);
          | None => print_endline (printGrammar grammar name);
        }
      } else {
        print_endline (Json.result_to_string result);
      }
    }
  };
};

let parseOptions options => {
  let htbl = Hashtbl.create 13;
  let rec loop options => {
    switch options {
      | [] => ()
      | [one] => failwith "Options must be in pairs"
      | [key, value, ...rest] =>  {
        Hashtbl.replace htbl key value;
        loop rest;
      }
    }
  };
  loop options;
  htbl
};

let getGrammar filename => {
  let contents = Sysop.readFile filename;
  switch (Runtime.parse GrammarGrammar.grammar "Start" contents) {
    | PackTypes.Result.Failure maybeResult (charsParsed, failure) => {
      Printf.eprintf "%s\n" (PackTypes.Error.genErrorText contents failure);
      exit 1;
    }
    | PackTypes.Result.Success result => {
      GrammarOfGrammar.convert result;
    }
  }
};

let examples filename ruleName options => {
  let grammar = getGrammar filename;
  let table = parseOptions options;
  print_endline (ExampleGenerator.generateExamples grammar ruleName table);
};

let ocamlBinaryParser use_stdin filename => {
  let chan = switch use_stdin {
    | true => stdin
    | false => {
        let file_chan = open_in filename;
        seek_in file_chan 0;
        file_chan;
      }
  };
  ignore (really_input_string chan (String.length Config.ast_impl_magic_number));
  ignore (input_value chan);
  input_value chan;
};

let optOr orr opt => switch opt { | None => orr | Some x => x };

let prettyBin filename binName => {
  let ast = ocamlBinaryParser false binName;
  let grammar = getGrammar filename;
  let result = OcamlOfReason.convertFrom ast;
  print_endline (PrettyPrint.toString grammar result |> optOr "Unable to pretty print this");
};

let tests cases => {
  print_endline "[[ TESTING ]]";
  (List.iter
  (fun (rule, text) => {
    switch (Runtime.parse GrammarGrammar.grammar rule text) {
      | PackTypes.Result.Failure maybeResult (charsParsed, failure) => {
        print_string (PackTypes.Error.genErrorText text failure);
      }
      | PackTypes.Result.Success result => ()
    };
  })
  cases);
  Printf.printf "[[ DONE %d ]]\n" (List.length cases);
};

let testCases = [
  ("ident", "Start"),
  ("white", " "),
  ("eol", "     \t\n"),
  ("eol", "     \t"),
  ("ItemInner", "Rule"),
  ("ItemInner", "(Rule #eol)"),
  ("Item", "(Rule #eol)"),
  ("Item", "Rule?"),
  ("Choice", "(Rule #eol)* Rule?"),
  ("Rule", "Start = (Rule #eol)* Rule?"),
  ("Rule", "Start = \n | (Rule #eol)* Rule?"),
  ("Start", "Start = \n | (Rule #eol)* Rule?\n | Other\n\nMore = thing"),
  ("Comment_eol", "; something\n\n"),
  ("Rule", "\nStart = 'h'"),
  ("Rule", "\n\nStart = ;hi\n | 'h'"),
  ("Start", "\n\nStart = ;hi\n | 'h'"),
  ("Start", {|
Start = Rule*
; something
Rule = ;hi
  | eol? [name]ident "=" [choices]Choice #eol
  | eol? [name]ident "=" #eol ("|" [choices]Choice #eol)+

Choice = [children]Item+ ("--" [name]ident)? (";" #[comment]rest_of_line)?

Item = [?neg]"~"? [?lexify]"#"? ("[" [flag]flag? [name]ident "]")? [inner]ItemInner [suffix]suffix?

ItemInner =
  | string
  | ident
  | "(" [:nested]Item+ ")" -- nested
  | char_range
  | char

char_range = "'" [start]single ".." [end]single "'"
char = "'" [char]single "'"

single =
  | "\\" any
  | ~"'" ~'\n' any

string = '"' [@contents]strchar* '"'
strchar =
  | "\\" any
  | ~'"' ~'\n' any

flag =
  | "?" -- bool ; exists
  | ":" -- array
  | "@" -- string ; contents

suffix =
  | "+"
  | "*"
  | "?"

ident = ~"0" identchar+
identchar =
  | 'a..z'
  | 'A..Z'
  | '0..9'
  | '_'

rest_of_line = (~"\n" any)*

eol = white* eee
eee =
  | eolchar+
  | EOF
eolchar =
  | "\n"
  | "\r"
white =
  | " "
  | "\t"

|}),
  ("Item", "Hello"),
];

switch (Array.to_list Sys.argv) {
  | [_, "test"] => tests testCases;
  | [_, "dump"] => main dump::true ()
  | [_, "dump", filename] => main dump::true file::filename ()
  | [_, "dump", filename, destination] => main dump::true file::filename dest::destination ()
  | [_, "examples", filename, ruleName, ...rest] => examples filename ruleName rest
  | [_, "pretty-bin", filename, binName] => prettyBin filename binName
  | [_, filename] => main dump::false file::filename ()
  | _ => main()
};
