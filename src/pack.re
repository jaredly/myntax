
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
  /* To support backtracking, need to read all input. */
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
    } else if (i < state.len) {
      Printf.eprintf ">>>>\n";
      Printf.eprintf "parse error: didn't consume all '%s' \"%s\" (just \"%s\")\n" rule text (String.sub text 0 i);
      /* Printf.printf "%s" (Yojson.Safe.to_string(result_to_yojson result)); */
      /* Printf.printf "%s" (Yojson.Safe.to_string(result_to_yojson result)); */
    };
  })
  cases);
  print_endline "[[ DONE ]]";
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
  ("Newl", "Start \n more"),
  ("Rule", "Start = \n | (Rule #eol)* Rule?"),
  ("Start", "Start = \n | (Rule #eol)* Rule?\n | Other\n\nMore = thing"),
  ("Item", "Hello"),
];

if (Array.length Sys.argv >= 2 && Sys.argv.(1) == "test") {
  tests testCases;
} else {
  main();
};
