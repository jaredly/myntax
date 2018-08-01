
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
let unwrapWith = (message, opt) => switch opt {
  | None => failwith(message)
  | Some(x) => x
};

let loc = H.default_loc^;

let str = H.Str.eval(H.Exp.array([]));

type loc = PackTypes.Result.loc;

type fromOcaml = {
  fromStructure: (fromOcaml, structure_item) => result,
  fromExpression: (fromOcaml, expression) => result
};

type toOcaml = {
  toLoc: (loc) => Location.t,
  expression: (toOcaml, (string, list((string, result)), loc)) => expression,
  structure: (toOcaml, (string, list((string, result)), loc)) => structure_item
};

let sliceToEnd = (s, start) => {
  let l = String.length(s);
  start <= l ? String.sub(s, start, l - start) : ""
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
};

/* Types */

let parseCoreType = (toOcaml, (sub, children, loc)) => switch sub {
  | "constr_no_args" => {
    let name = RU.getNodeByType(children, "longIdent") |> unwrap |> parseLongIdent;
    H.Typ.constr(Location.mkloc(name, toOcaml.toLoc(loc)), [])
  }
  | _ => failwith("unhandled core type sub " ++ sub)

};

let parseTypeKind = (toOcaml, (sub, children, loc)) => {
  switch sub {
    | "record" => {
      Ptype_record(
        RU.getNodesByType(children, "TypeObjectItem", ((sub, children, loc)) => {
          let (name, nameLoc) = {
            let (_, children, loc) = RU.getNodeByType(children, "shortAttribute") |> unwrap;
            (RU.getContentsByType(children, "lowerIdent") |> unwrap, loc)
          };
          let t = sub == "punned"
          ? H.Typ.constr(Location.mkloc(Lident(name), toOcaml.toLoc(nameLoc)), [])
          : parseCoreType(toOcaml, RU.getNodeByType(children, "CoreType") |> unwrap)
          let name = Location.mkloc(name, toOcaml.toLoc(loc));
          H.Type.field(
            name,
            t
          )
        })
      )
    }
    | _ => failwith("Unsupported type kind " ++ sub)
  }
};

/* Patterns */

let rec parsePattern = (toOcaml, (sub, children, loc)) => {
  let ocamlLoc = toOcaml.toLoc(loc);
  switch sub {
    | "ident" => H.Pat.var(~loc=ocamlLoc, Location.mkloc(RU.getContentsByType(children, "lowerIdent") |> unwrap, toOcaml.toLoc(loc)))
    | "object" => {
      H.Pat.record(
        ~loc=ocamlLoc,
        RU.getNodesByType(children, "PatternObjectItem", ((sub, children, loc)) => {
          let (name, nameLoc) = {
            let (_, children, loc) = RU.getNodeByType(children, "attribute") |> unwrapWith("No attribute");
            (RU.getNodeByType(children, "longIdent") |> unwrapWith("No longident") |> parseLongIdent, loc)
          };
          let t = sub == "punned"
          ? H.Pat.var(Location.mkloc(Longident.last(name), toOcaml.toLoc(nameLoc)))
          : parsePattern(toOcaml, RU.getNodeByType(children, "Pattern") |> unwrapWith("No pat"));
          (Location.mkloc(name, toOcaml.toLoc(nameLoc)), t)
        }),
        Open
      )
    }
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

let parseArrow = (toOcaml, (sub, children, loc)) => {
  let res = RU.getNodeByType(children, "Expression") |> unwrapWith("no expr") |> toOcaml.expression(toOcaml);
  let (sub, children, loc) = RU.getNodeByType(children, "FnArgs") |> unwrapWith("no args");
  switch sub {
    | "single" => H.Exp.fun_(
      "",
      None,
      H.Pat.var(
        Location.mknoloc(RU.getContentsByType(children, "lowerIdent") |> unwrapWith("no ident"))
      ),
      res
    )
    | "unit" => H.Exp.fun_(
      "",
      None,
      H.Pat.construct(Location.mknoloc(Lident("()")), None),
      res
    )
    | "ignored" => H.Exp.fun_(
      "",
      None,
      H.Pat.any(),
      res
    )
    | "multiple" => {
      let rec loop = (items, res) => switch items {
        | [] => res
        | [(sub, children, loc), ...rest] => {
          let (pattern, label) = switch sub {
            | "unlabeled" => {
              (parsePattern(toOcaml, RU.getNodeByType(children, "Pattern") |> unwrapWith("No pat")), None);
            }
            | "labeled" => {
              let label = RU.getContentsByType(children, "argLabel") |> unwrapWith("No label");
              let label = sliceToEnd(label, 1);
              let pattern = H.Pat.var(Location.mkloc(label, toOcaml.toLoc(loc)));
              (pattern, Some(label))
            }
            | _ => failwith("Unhandled pattern kind " ++ sub)
          };
          loop(rest, H.Exp.fun_(
            label |? "",
            None,
            pattern,
            res
          ))
        }
      };
      loop(
        RU.getNodesByType(children, "FnArg", x => x),
        res
      )
    }
    | _ => failwith("Can't parse this arrow")
  }
};

let parseExpression = (toOcaml, (sub, children, loc)) => {

  let oloc = toOcaml.toLoc(loc);
  switch sub {
    | "array_index" => failexpr("Array index not done")
    | "fn_call" => H.Exp.apply(
        ~loc=oloc,
        toOcaml.expression(toOcaml, RU.getNodeByLabel(children, "fn") |> unwrap |> stripRuleName),
        RU.getNodesByType(children, "FnCallArg", ((sub, children, loc)) => {
          switch sub {
            | "expr" => ("", toOcaml.expression(toOcaml, RU.getNodeByType(children, "Expression") |> unwrap))
            | "labeled" => {
              let name = RU.getContentsByType(children, "argLabel") |> unwrapWith("no arg label");
              let name = sliceToEnd(name, 1);
              let expr = toOcaml.expression(toOcaml, RU.getNodeByType(children, "Expression") |> unwrapWith("No expr"));
              (name, expr)
            }
            | "punned" => {
              let name = RU.getContentsByType(children, "argLabel") |> unwrapWith("No pun");
              let name = sliceToEnd(name, 1);
              (name, H.Exp.ident(Location.mkloc(Lident(name), toOcaml.toLoc(loc))))
            }
            | _ => failwith("Unvalid fncallarg sub " ++ sub)
          }
        })
      )

    /* Special lispisms */

    | "threading_last" => {
      let target = RU.getNodeByLabel(children, "target") |> unwrap |> stripRuleName |> toOcaml.expression(toOcaml);
      let items = RU.getNodesByType(children, "ThreadItem", x => x);
      let rec loop = (target, items) => switch items {
        | [] => target
        | [("ident", children, _), ...rest] => 
          loop(H.Exp.apply(
        ~loc=oloc,
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
    | "attribute" => {
      let (name, nameLoc) = {
        let (_, children, loc) = RU.getNodeByType(children, "attribute") |> unwrapWith("No attribute");
        (RU.getNodeByType(children, "longIdent") |> unwrapWith("No longident") |> parseLongIdent, loc)
      };
      H.Exp.fun_(
        ~loc=oloc,
        "",
        None,
        H.Pat.var(Location.mknoloc("x")),
        H.Exp.field(
          H.Exp.ident(Location.mknoloc(Lident("x"))),
          Location.mkloc(name, toOcaml.toLoc(nameLoc))
        )
      )
    }

    /* Basics */
    | "op" => H.Exp.ident(
        ~loc=oloc,
      Location.mkloc(Lident(RU.getContentsByType(children, "operator") |> unwrap), toOcaml.toLoc(loc)))
    | "ident" => H.Exp.ident(
        ~loc=oloc,
      Location.mkloc(
        parseLongIdent(RU.getNodeByType(children, "longIdent") |> unwrap), toOcaml.toLoc(loc)))

    | "arrow" => parseArrow(toOcaml, RU.getNodeByType(children, "Arrow") |> unwrap)
    | "record_attribute" => {
      let (name, nameLoc) = {
        let (_, children, loc) = RU.getNodeByType(children, "attribute") |> unwrapWith("No attribute");
        (RU.getNodeByType(children, "longIdent") |> unwrapWith("No longident") |> parseLongIdent, loc)
      };
      H.Exp.field(
        ~loc=oloc,
        RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml),
        Location.mkloc(name, toOcaml.toLoc(nameLoc))
      )
    }

    /* Constructors */

    | "const" => H.Exp.constant(
      ~loc=oloc,
      parseConstant(RU.getNodeByType(children, "constant") |> unwrap))
    | "array_literal" =>
      listToConstruct
        (
          RU.getNodesByLabel(children, "items", toOcaml.expression(toOcaml)),
          RU.getNodeByLabel(children, "spread")
          |?>> stripRuleName
          |?>> toOcaml.expression(toOcaml),
          H.Exp.construct,
          H.Exp.tuple
        )
    | "object_literal" =>
      H.Exp.record(
        ~loc=oloc,
        RU.getNodesByType(children, "ObjectItem", ((sub, children, loc)) => {
          let (name, nameLoc) = {
            let (_, children, loc) = RU.getNodeByType(children, "attribute") |> unwrapWith("No attribute");
            (RU.getNodeByType(children, "longIdent") |> unwrapWith("No longident") |> parseLongIdent, loc)
          };
          let t = sub == "punned"
          ? H.Exp.ident(Location.mkloc(Lident(Longident.last(name)), toOcaml.toLoc(nameLoc)))
          : toOcaml.expression(toOcaml, RU.getNodeByType(children, "Expression") |> unwrapWith("No expr"));

          (Location.mkloc(name, toOcaml.toLoc(nameLoc)), t)
        }),
        RU.getNodeByLabel(children, "spread") |?>> stripRuleName |?>> toOcaml.expression(toOcaml)
      )
    | _ => failexpr("Unexpected expression type: " ++ sub)
  };
};

/* Structures */

let parseLetPair = (toOcaml, (sub, children, loc)) => {
  let pat = RU.getNodeByType(children, "Pattern") |> unwrapWith("no pattern") |> parsePattern(toOcaml);
  let init = RU.getNodeByType(children, "Expression") |> unwrapWith("no expr") |> toOcaml.expression(toOcaml);
  H.Vb.mk(pat, init)
};

let parseStructure = (toOcaml, (sub, children, loc)) => {
  switch sub {
    | "module" => {
      let name = RU.getContentsByType(children, "capIdent") |> unwrap;
      let (sub, children, loc) = RU.getNodeByType(children, "ModuleExpr") |> unwrap;
      let desc = switch sub {
        | "structure" => {
          let items = RU.getNodesByType(children, "Structure", toOcaml.structure(toOcaml));
          Pmod_structure(items)
        }
        | _ => failwith("Unhandled module type")
      };
      H.Str.module_(
        H.Mb.mk(
          Location.mkloc(name, toOcaml.toLoc(loc)),
          H.Mod.mk(desc)
        )
      )
    }
    | "eval" => H.Str.eval(RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml))
    | "type" => H.Str.type_(
      RU.getNodesByType(children, "TypePair", ((sub, children, loc)) => {
        let (name, vbls) = {
          let (sub, children, loc) = RU.getNodeByType(children, "TypeName") |> unwrap;
          let name = RU.getContentsByType(children, "lowerIdent") |> unwrap;
          switch (sub) {
            | "plain" => (name, [])
            | "vbl" => (name, RU.getManyContentsByType(children, "typeVariable"))
            | _ => failwith("Invalid typ typ")
          }
        };
        let kind = parseTypeKind(toOcaml, RU.getNodeByType(children, "TypeDecl") |> unwrap);
        H.Type.mk(
          ~params=vbls |> List.map(name => (H.Typ.var(name), Invariant)),
          ~kind,
          Location.mkloc(name, toOcaml.toLoc(loc))
        )
      })

    )
    | "open" => H.Str.open_(H.Opn.mk(
      Location.mkloc(
        parseLongCap(RU.getNodeByType(children, "longCap") |> unwrap),
        toOcaml.toLoc(loc)
      )
    ))
    | "let_rec" => H.Str.value(Recursive, RU.getNodesByType(children, "LetPair", (pair) => {
      parseLetPair(toOcaml, pair)
    }))
    | "let" => H.Str.value(Nonrecursive, [parseLetPair(
      toOcaml,
      RU.getNodeByType(children, "LetPair") |> unwrapWith("no let pair")
    )])
    | _ => Ast_helper.Str.eval(failexpr("Unexpected sub: " ++ sub))
  }
};



let toOcaml = {toLoc: l => Location.none, structure: parseStructure, expression: parseExpression};

let calcBols = text => {
  let lines = Str.split(Str.regexp_string("\n"), text);
  let (_, bols) = Belt.List.reduce(lines, (0, []), ((offset, results), line) => {
    (
      offset + String.length(line) + 1,
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
  loop(1, bols)
};

let lexingPos = (fname, offset, bols) => {
  /* let (lno, bol) = parsingPos(offset, bols); */
  /* {Lexing.pos_lnum: lno, pos_bol: bol, pos_cnum: offset, pos_fname: fname} */
  offset
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










