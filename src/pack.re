
let getStdin () => {
  let lines = ref [];
  try (
    while true {
      lines := [read_line (), ...!lines]
    }
  ) {
  | End_of_file => ()
  };
  let lines = List.rev !lines;
  String.concat "\n" lines
};

let main () => {
  let initialRule = "Start";
  let state = Runtime.initialState (getStdin ());
  let (i, result) = Runtime.apply_rule GrammarGrammar.grammar state initialRule 0;

  if (i == (-1)) {
    /* TODO: report errors! */
    Printf.eprintf "parse error: parsing failed\n";
    exit 1
  } else if (i < state.len) {
    Printf.printf "%s\n" (Json.result_to_string result);
    Printf.eprintf "parse error: extra characters after end of input \"%s\"\n" (String.sub state.input i (state.len - i));
    exit 1
  } else {
    Printf.printf "%s" (Json.result_to_string result);
    /* Printf.printf "parsed OK\n" */
  }
};

let tests cases => {
  print_endline "[[ TESTING ]]";
  (List.iter
  (fun (rule, text) => {
    let state = Runtime.initialState text;
    let (i, result) = Runtime.apply_rule GrammarGrammar.grammar state rule 0;
    if (i == -1) {
      Printf.eprintf ">>>>\n";
      Printf.eprintf "parse error: parsing failed for '%s' \"%s\"\n" rule text;
    } else if (i < state.Runtime.len) {
      Printf.eprintf ">>>>\n";
      Printf.eprintf "parse error: didn't consume all '%s' \"%s\" (just \"%s\")\n" rule text (String.sub text 0 i);
      /* Printf.printf "%s" (Yojson.Safe.to_string(result_to_yojson result)); */
      /* Printf.printf "%s" (Yojson.Safe.to_string(result_to_yojson result)); */
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
  ("Comment", "; something"),
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

if (Array.length Sys.argv >= 2 && Sys.argv.(1) == "test") {
  tests testCases;
} else {
  main();
};
