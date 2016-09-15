let module Parsing = {
  type ignoreNewlines =
    | Yes
    | No
    | Inherit [@@deriving show];
  type grammar = {
    lineComment: option string,
    blockComment: option (string, string),
    rules: list (string, rule)
  }
  and rule = {
    passThrough: bool,
    ignoreNewlines: ignoreNewlines,
    leaf: bool,
    choices: list choice,
  }
   /* rule name -> e1 | e2 | ... */
  /* -> add "pass-through" and "ignore-whitespace" */
  and choice = (string, string, list parsing) /* choice name, comment, sequence */
  and parsing =
    | Star parsing (option string)      /* e* */
    | Plus parsing (option string)      /* e+ */
    | Optional parsing (option string)  /* e? */
    | Any (option string) /* any */
    | NoSpaceAfter parsing /* a printing rule, suppresses space */
    | NoSpaceBefore parsing /* a printing rule, suppresses space */
    | EOF /* EOF */
    | CommentEOL
    | Group (list parsing)  /* ( e ... ) */
    | Lookahead parsing  /* &e */
    | Not parsing        /* !e */
    | Lexify parsing     /* # somelexrule */
    | NonTerminal string (option string)/* nonterminal 'name' */
    | Terminal string (option string)   /* terminal */
    | Chars char char (option string)   /* [a-z] */
    | Empty              /* epsilon */[@@deriving show];

};

/* let module NewParsing = {
  open Parsing;
  type grammar = {
    lineComment: option string,
    blockComment: option (string, string),
    rules: list (string, rule)
  }
  and rule = {
    passThrough: bool,
    ignoreNewlines: ignoreNewlines,
    leaf: bool,
    choices: list choice,
  }[@@deriving show];
}; */

let unwrapOr a b => {
  switch a {
    | Some x => x
    | None => b
  }
};

let module Path = {
  type pathItem =
    | Item Parsing.parsing int
    | Iter int
    | Choice int string [@@deriving show];
};

let module Error = {

  type errors = (int, list (bool, list Path.pathItem)) [@@deriving show];
  type partial = (int, errors) [@@deriving show];

  let errorText (isNot, rule) => {
    switch rule {
      | Parsing.Terminal text label => "Expected \"" ^ (String.escaped text) ^ "\""
      | Parsing.Chars start cend label => Printf.sprintf "Expected %c..%c" start cend
      | Parsing.NonTerminal name label => name
      | Parsing.Any label => "Any"
      | Parsing.Star _ label => "Star"
      | Parsing.Plus _ label => "Plus"
      | Parsing.Optional _ label => "Optional"
      | Parsing.EOF => "End of Input"
      | Parsing.CommentEOL => "Expected a newline (with optional comments)"
      | _ => "Unknown problem"
    }
  };

  let errorPathItemText isNot pathItem => {
    switch pathItem {
      | Path.Choice n name => (string_of_int n) ^ ":" ^ name
      | Path.Item item loopIndex => (string_of_int loopIndex) ^ ":" ^ (errorText (isNot, item)) /*^ " [" ^ (string_of_int loopIndex) ^ "]"*/
      | Path.Iter n => "*" ^ (string_of_int n)
    }
  };

  let rec errorPathText isNot path collect => {
    switch path {
      | [] => collect
      | [pathItem, ...path] => (errorPathText isNot path [errorPathItemText isNot pathItem, ...collect])
    }
  };

  let lastLineLength txt pos => {
    if (pos >= String.length txt) {
      String.length txt
    } else {
    try {
      let atNewline = String.get txt pos == '\n';
      let mpos = atNewline ? pos - 1 : pos;
      let lastPos = String.rindex_from txt mpos '\n';
      pos - lastPos - 1
    } {
      | Not_found => pos
    }
  }
  };

  let leftPad base num coll => {
    let res = ref "";
    for i in 0 to num {
      res := base ^ !res;
    };
    !res
  };


  let genErrorText text (pos, errors) => {
    let showText = String.sub text 0 {
      try (String.index_from text pos '\n')
      {
        | Not_found => (String.length text)
      }
    };
    (Printf.sprintf "%s\n%s^\n" showText (leftPad "-" ((lastLineLength text pos) - 1) ""))
    ^
    (String.concat "" (List.map (fun (isNot, errPath) => {
      Printf.sprintf "%s\n" (String.concat " > " (errorPathText isNot errPath []))
    }) errors));
  };
};

let module Result = {
  /* type resultType =
    | Terminal string
    | Lexical (string, string, int) string bool
    | Nonlexical (string, string, int) bool [@@deriving (yojson, show)]; */

  type rule = (string, string) [@@deriving (yojson, show)];
  type loc = (int, int) [@@deriving (yojson, show)];
  type result =
    | Leaf rule string loc
    | Node rule (list (string, result)) loc /* label, child */
  [@@deriving (yojson, show)]
  ;

  /* let resultTypeDescription rt => switch rt {
    | Terminal s => "Terminal(" ^ s ^ ")"
    | Lexical (name, sub, index) text passThrough => "Lexical(" ^ name ^ "," ^ text ^ "," ^ (string_of_int index) ^ ")"
    | Nonlexical (name, sub, index) passThrough => "Nonlexical(" ^ name ^ "," ^ (string_of_int index) ^ ")"
  }; */

  /* type result = {
    start: int,
    cend: int,
    typ: resultType,
    label: option string,
    children: list result,
  } [@@deriving (yojson, show)];
 */

  /* type partial = {
    path: list string,
    expected: string,
    position: int,
    lno: int,
    cno: int,
  } [@@deriving yojson]; */

  type parserMatch =
    | Success result
    | Failure (option result) Error.partial;

};


/* include Parsing; */
/* include Result; */
