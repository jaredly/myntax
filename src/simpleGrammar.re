
open PackTypes;

let nt name => NonTerminal name None;
let t name => Terminal name None;
let chars start cend => (Chars start cend None);


let grammar: grammar = [
  /*
     start -> expr
   */
  ("Start", [("", "", [nt "Expr"])]),
  /*
     expr ->
           | expr expr
           | "fun" patt "->" expr = "fundecl" ; function
           | "(" expr ")"
           | expr "+" expr
           | ident
           | int64
   */
  (
    "Expr",
    [
      ("funappl", "", [nt "Expr", nt "sp", nt "Expr"]),
      (
        "fundecl",
        "function declaration",
        [t "fun", nt "sp", nt "Patt", nt "osp", t "->", nt "sp", nt "Expr"]
      ),
      ("paren", "", [t "(", nt "osp", nt "Expr", nt "osp", t ")"]),
      ("addition", "", [nt "Expr", nt "osp", t "+", nt "osp", nt "Expr"]),
      ("ident", "", [nt "ident"]),
      ("int64", "", [nt "int64"])
    ]
  ),
  /*
     patt -> ident
   */
  ("Patt", [("", "", [nt "ident"])]),
  /*
     int64 -> digit19 digit*
            | digit
     digit -> [0-9]
     digit19 -> [1-9]
   */
  (
    "int64",
    [
      /* ("", "", [nt "digit19", Star (nt "digit")]) */
      ("", "", [Not (t "0"), Plus (nt "digit") None])
      /* ("", "", [Plus(nt "digit")]) */
    ]
  ),
  ("digit", [("", "", [chars '0' '9'])]),
  ("digit19", [("", "", [chars '1' '9'])]),
  /*
     ident -> alpha+
     alpha -> [a-z]
   */
  ("ident", [("", "", [Not (nt "reserved"), Plus (nt "alpha") None])]),
  ("reserved", [
    ("", "", [t "let"]),
    ("", "", [t "for"]),
    ("", "", [t "fun"]),
  ]),
  ("alpha", [("", "", [chars 'a' 'z'])]),
  /*
     sp -> " "+
     osp -> " "*
   */
  ("sp", [("", "", [Plus (t " ") None])]),
  ("osp", [("", "", [Star (t " ") None])])
];
