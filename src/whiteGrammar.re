
open PackTypes;

let nt label::label=? name => NonTerminal name label;
let t label::label=? name => Terminal name label;
let chars label::label=? start cend => (Chars start cend label);


let grammar: grammar = [
  /*
     start -> expr
   */
  ("Start", [("", "", [Star (nt "Item") None])]),
  /*
     expr ->
           | expr expr
           | "fun" patt "->" expr = "fundecl" ; function
           | "(" expr ")"
           | expr "+" expr
           | ident
           | int64
   */
   ("Item", [
     ("num", "", [nt "int64"]),
     ("parens", "", [t "(", Star (nt "Item") (Some "contents"), t ")"]),
     ("interleaved", "", [Plus (Group [t "=", t "a"]) None]),
     ("ident", "", [nt "ident"]),
   ]),
  (
    "int64",
    [
      ("", "", [Not (t "0"), Plus (nt "digit") None])
    ]
  ),
  ("digit", [("", "", [chars '0' '9'])]),
  ("ident", [("", "", [Not (nt "reserved"), Plus (nt "alpha") None])]),
  ("reserved", [
    ("", "", [t "let"]),
    ("", "", [t "for"]),
    ("", "", [t "fun"]),
  ]),
  ("alpha", [("", "", [chars 'a' 'z'])]),
];
