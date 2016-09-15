
open PackTypes.Parsing;

let nt label::label=? name => NonTerminal name label;
let t label::label=? name => Terminal name label;
let chars label::label=? start cend => (Chars start cend label);
let st label::label=? child => Star child label;
let pl label::label=? child => Plus child label;
let opt label::label=? child => Optional child label;

let grammar: grammar = [
  /*
  Start = (Rule #eol)* Rule?
   */
  ("Start", [("", "", [st (nt "Rule")])]),

  ("Rule", [
    ("", "", [(opt (nt "eol")), nt label::"name" "ident", t "=", nt label::"choices" "Choice", Lexify (nt "eol")]),
    ("", "", [
      (opt (nt "eol")),
      nt label::"name" "ident",
      t "=",
      Lexify (nt "eol"),
      pl (Group [t "|", nt label::"choices" "Choice", Lexify (nt "eol")])
    ])
  ]),

  ("Newl", [
    ("", "", [nt label::"name" "ident", Lexify (nt "eol"), nt "ident"])
  ]),

  ("Choice", [
    ("", "", [
      pl (nt label::"children" "Item"),
      opt (Group [t "--", nt label::"name" "ident"]),
      opt (Group [t ";", Lexify (nt label::"comment" "rest_of_line")]),
    ]),
  ]),

  ("Item", [
    ("", "", [
      opt (t label::"neg" "~"),
      opt (t label::"lexify" "#"),
      opt (Group [
        t "[",
        opt (nt label::"flag" "flag"),
        nt label::"name" "ident",
        t "]",
      ]),
      nt label::"inner" "ItemInner",
      opt (nt label::"suffix" "suffix"),
    ])
  ]),

  ("ItemInner", [
    ("", "", [nt "string"]),
    ("", "", [nt "char_range"]),
    ("", "", [nt "char"]),
    ("", "", [nt "ident"]),
    ("nested", "", [t "(", pl (nt label::"nested" "Item"), t ")"]),
  ]),

  ("char_range", [
    ("", "", [
      t "'",
      nt label::"start" "single",
      t "..",
      nt label::"end" "single",
      t "'",
    ])
  ]),

  ("char", [
    ("", "", [
      t "'",
      nt label::"char" "single",
      t "'",
    ])
  ]),

  ("single", [
    ("", "", [t "\\", Any None]),
    ("", "", [Not (t "'"), Not (t "\n"), Any None]),
  ]),

  ("string", [
    ("", "", [
      t "\"",
      st (nt label::"contents" "strchar"),
      t "\"",
    ])
  ]),

  ("strchar", [
    ("", "", [t "\\", Any None]),
    ("", "", [Not (t "\""), Not (t "\n"), Any None]),
  ]),

  ("flag", [
    ("bool", "exists", [t "?"]),
    ("array", "", [t ":"]),
    ("string", "contents", [t "@"]),
  ]),

  ("suffix", [
    ("plus", "", [t "+"]),
    ("star", "", [t "*"]),
    ("opt", "", [t "?"]),
  ]),

  ("ident", [
    ("", "", [Not (t "0"), pl (nt "identchar")]),
  ]),

  ("identchar", [
    ("", "", [Chars 'a' 'z' None]),
    ("", "", [Chars 'A' 'Z' None]),
    ("", "", [Chars '0' '9' None]),
    ("", "", [t "_"]),
  ]),

  ("rest_of_line", [
    ("", "", [st (Group [Not (nt "eolchar"), Any None])])
  ]),

  ("eol", [
    ("", "", [st (nt "white"), nt "eee"])
  ]),

  ("eee", [("", "", [pl (nt "eolchar")]), ("", "", [EOF])]),
  ("eolchar", [
    ("", "", [t "\n"]),
    ("", "", [t "\r"]),
  ]),

  ("white", [
    ("", "", [t " "]),
    ("", "", [t "\t"]),
    ("", "", [t "\v"]),
  ]),
];
