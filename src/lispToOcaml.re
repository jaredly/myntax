
/* ResultUtils */
open Parsetree;

open PackTypes.Result;

open Longident;

open Location;

open Lexing;

open Asttypes;

open Infix;

module H = Ast_helper;

module RU = ResultUtils;

let unwrap = ResultUtils.unwrap;

let loc = H.default_loc^;

let str = H.Str.eval(H.Exp.array([]));

type loc = (int, int);

type fromOcaml = {
  fromStructure: (fromOcaml, structure_item) => result,
  fromExpression: (fromOcaml, expression) => result
};

type toOcaml = {
  toLoc: (loc) => Location.t,
  expression: (toOcaml, (string, list((string, result)), loc)) => expression,
  structure: (toOcaml, (string, list((string, result)), loc)) => structure_item
};

let stripQuotes = (str) => String.sub(str, 1, String.length(str) - 2);
let processString = (str) => str |> stripQuotes |> Scanf.unescaped;

let failexpr = message => H.Exp.apply(
  H.Exp.ident(Location.mknoloc(Longident.Lident("failwith"))),
  [("", Ast_helper.Exp.constant(Const_string(message, None)))]
);

let stripRuleName = (((name, sub), children, loc)) => (sub, children, loc);


/* Utils */

let rec parseLongCap_ = ((sub, children, _)) =>
  switch sub {
  | "lident" => Lident(RU.getContentsByType(children, "capIdent") |> unwrap)
  | "dot" =>
    Ldot(
      RU.getNodeByType(children, "longCap_") |> unwrap |> parseLongCap_,
      RU.getContentsByType(children, "capIdent") |> unwrap
    )
  | _ => failwith("Invalid longCap_ sub " ++ sub)
  };

let parseLongCap = ((_, children, _)) =>
  RU.getNodeByType(children, "longCap_") |> unwrap |> parseLongCap_;

let parseLongIdent = ((_, children, _)) => {
  let first = RU.getNodeByType(children, "longCap_") |?>> parseLongCap_;
  let last = RU.getContentsByType(children, "lowerIdent") |> unwrap;
  switch first {
  | Some(x) => Ldot(x, last)
  | None => Lident(last)
  }
  /* let leafs = RU.getChildren children (fun (_, child) => {
       switch child {
         | Leaf ("lowerIdent", _) contents _ => Some contents
         | Leaf ("capIdent", _) contents _ => Some contents
         | _ => None
       }
     });
     let rec loop leafs current => switch leafs {
       | [contents, ...rest] => loop rest (Ldot current contents)
       | [] => current
     };
     switch leafs {
       | [leftMost, ...rest] => {
         loop rest (Lident leftMost)
       }
       | [] => failwith "empty longident"
     }
     /* loop leafs */ */
};


/* Types */


/* Patterns */

let parsePattern = (toOcaml, (sub, children, loc)) => {
  switch sub {
    | "ident" => H.Pat.var(Location.mkloc(RU.getContentsByType(children, "lowerIdent") |> unwrap, toOcaml.toLoc(loc)))
    | _ => failwith("Not supportdd pattern sub: " ++ sub)
  }
};


/* Expressions */

let parseConstant = ((sub, children, loc)) => {
  let raw = RU.getContentsByLabel(children, "val") |> unwrap;
  switch sub {
    | "string" => Const_string(processString(raw), None)
    | "int" => Const_int(int_of_string(raw))
    | "float" => Const_float(raw)
    | _ => failwith("Unhandled constant type")
  }
};

let rec listToConstruct = (list, maybeRest, typeC, tupleC) =>
  switch list {
  | [] =>
    switch maybeRest {
    | None => typeC(Location.mkloc(Lident("[]"), loc), None)
    | Some(x) => x
    }
  | [one, ...rest] =>
    typeC(
      Location.mkloc(Lident("::"), loc),
      Some(tupleC([one, listToConstruct(rest, maybeRest, typeC, tupleC)]))
    )
  };

let parseExpression = (toOcaml, (sub, children, loc)) =>
  switch sub {
    | "array_index" => failexpr("Array index not done")
    | "fn_call" => H.Exp.apply(
      toOcaml.expression(toOcaml, RU.getNodeByLabel(children, "fn") |> unwrap |> stripRuleName),
      RU.getNodesByLabel(children, "args", toOcaml.expression(toOcaml))
      |> List.map(m => ("", m))
    )
    | "threading_last" => {
      let target = RU.getNodeByLabel(children, "target") |> unwrap |> stripRuleName |> toOcaml.expression(toOcaml);
      let items = RU.getNodesByType(children, "ThreadItem", x => x);
      let rec loop = (target, items) => switch items {
        | [] => target
        | [("ident", children, _), ...rest] => 
          loop(H.Exp.apply(
            H.Exp.ident(Location.mkloc(parseLongIdent(RU.getNodeByType(children, "longIdent") |> unwrap), toOcaml.toLoc(loc))),
            [("", target)]
          ), rest)
        | [("fn_call", children, _), ...rest] => 
          loop(H.Exp.apply(
            toOcaml.expression(toOcaml, RU.getNodeByLabel(children, "fn") |> unwrap |> stripRuleName),
            (RU.getNodesByLabel(children, "args", toOcaml.expression(toOcaml))
            |> List.map(m => ("", m))) @ [("", target)]
          ), rest)
        | _ => failexpr("Unable to")
      };
      loop(target, items)
    }
    | "const" => H.Exp.constant(parseConstant(RU.getNodeByType(children, "constant") |> unwrap))
    | "op" => H.Exp.ident(Location.mkloc(Lident(RU.getContentsByType(children, "operator") |> unwrap), toOcaml.toLoc(loc)))
    | "ident" => H.Exp.ident(Location.mkloc(parseLongIdent(RU.getNodeByType(children, "longIdent") |> unwrap), toOcaml.toLoc(loc)))
    | "array_literal" =>
      listToConstruct
        (
          RU.getNodesByType(children, "Expression", toOcaml.expression(toOcaml)),
          None,
          /* RU.getContentsByLabel(children, "rest")
          |?> (label) => Some(H.Pat.var(Location.mkloc(label, oloc))), */
          H.Exp.construct,
          H.Exp.tuple
        )
    | _ => failexpr("Unexpected expression type: " ++ sub)
  };

/* Structures */

let parseLetPair = (toOcaml, (sub, children, loc)) => {
  let pat = RU.getNodeByType(children, "Pattern") |> unwrap |> parsePattern(toOcaml);
  let init = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
  H.Vb.mk(pat, init)
};

let parseStructure = (toOcaml, (sub, children, loc)) => {
  switch sub {
    | "eval" => Ast_helper.Str.eval(RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml))
    | "open" => H.Str.open_(H.Opn.mk(
      Location.mkloc(
        parseLongCap(RU.getNodeByType(children, "longCap") |> unwrap),
        toOcaml.toLoc(loc)
      )
    ))
    | "let" => H.Str.value(Nonrecursive, [parseLetPair(
      toOcaml,
      RU.getNodeByType(children, "LetPair") |> unwrap
    )])
    | _ => Ast_helper.Str.eval(failexpr("Unexpected sub: " ++ sub))
  }
};



let toOcaml = {toLoc: l => Location.none, structure: parseStructure, expression: parseExpression};

let calcBols = text => {
  let lines = Str.split(Str.regexp_string("\n"), text);
  let (_, bols) = Belt.List.reduce(lines, (0, []), ((offset, results), line) => {
    (
      offset + String.length(line),
      [offset, ...results]
    )
  });
  Belt.List.reverse(bols)
};

let parsingPos = (offset, bols) => {
  let rec loop = (i, bols) => switch bols {
    | [] => (i, 0)
    | [bol, next, ..._] when next > offset => (i, bol) 
    | [_, ...rest] => loop(i + 1, rest)
  };
  loop(0, bols)
};

let lexingPos = (fname, offset, bols) => {
  let (lno, bol) = parsingPos(offset, bols);
  {Lexing.pos_lnum: lno, pos_bol: bol, pos_cnum: offset, pos_fname: fname}
};

let convert = (result, fname, text) => {
  let bols = calcBols(text);

  switch result {
  | Node(("Start", _), children, _) =>
    RU.getNodesByType(children, "Structure", toOcaml.structure({
      ...toOcaml,
      /* TODO */
      toLoc: ((startPos, endPos)) => {
        Location.loc_start: lexingPos(fname, startPos, bols),
        loc_end: lexingPos(fname, endPos, bols),
        loc_ghost: false,
      }
    }))
  | _ => failwith("")
  };
};








/* [%rule
  "Structure";
  [
    (
    "let",
    {|"("& "let" LetPair &")"|},
    (~letPair) => H.Str.value(Nonrecursive, [letPair]),
    ),
    (
      "module",
      {|"("& "module" capIdent ModuleExpr &")"|},
      (~capIdent, ~moduleExpr) => H.Str.module_({
        pmb_name: Location.mkloc(getContents(capIdent)),
        etc,
      })
    )
  ];
];

[%rule
  "LetPair";
  {|[name]lowerIdent Expression|};
  (~name, ~expression) => {
    pvb_pat
  }
]





[@grammar {|
@lineComment(";")
@blockComment("(**", "*)")
Start = ModuleBody|}]
let convert = result => switch result {
  | Node(("Start", _), children, _) =>
    Ru.getNodesByType(children, "Structure", toOcaml.structure(toOcaml))
  | _ => assert(false)
} */










