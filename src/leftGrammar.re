
open PackTypes;

let nt label::label=? name => NonTerminal name label;
let t label::label=? name => Terminal name label;
let chars label::label=? start cend => (Chars start cend label);


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
      ("ha", "", [nt label::"fn" "Expr", nt "osp", t "ha"]),
      ("int64", "", [t "hello"])
    ]
  ),
];
