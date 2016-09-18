
/* ResultUtils */
open Parsetree;
open PackTypes.Result;
open Longident;
open Location;
open Lexing;
open Asttypes;
let module H = Ast_helper;
let module RU = ResultUtils;
let unwrap = ResultUtils.unwrap;

let loc = !H.default_loc;
let str = H.Str.eval (H.Exp.array []);

type loc = (int, int);

type fromOcaml = {
  fromStructure: fromOcaml => structure_item => result,
  fromExpression: fromOcaml => expression => result,
};

type toOcaml = {
  expression: toOcaml => (string, list (string, result), loc) => expression,
  structure: toOcaml => (string, list (string, result), loc) => structure_item,
};

let mLoc = (0, 0);
let mLeaf = Leaf ("", "") "" mLoc;

let isPresent opt => switch opt { | Some _ => true | None => false };

let stripQuotes str => {
  String.sub str 1 (String.length str - 2)
};

let isUpper x => {
  let n = Char.code (String.get x 0);
  90 >= n && n >= 65
};

/** LEXICAL THINGS **/
let processString str => {
  str |> stripQuotes |> Scanf.unescaped
};

let optFlatMap mapper opt => {
  switch opt {
    | Some x => mapper x
    | None => None
  }
};

let optMap mapper opt => {
  switch opt {
    | Some x => Some (mapper x)
    | None => None
  }
};

let getExpression toOcaml children => RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml;

/* let rec fromIdents longident coll => {
  switch longident {
    /* TODO lower vs caps */
    | Lident x => [("", Leaf (isUpper x ? "capIdent" : "lowerIdent", "") x mLoc), ...coll]
    | Ldot a b => fromIdents a [("", Leaf (isUpper b ? "capIdent" : "lowerIdent", "") b mLoc)]
    | Lapply a b => List.concat [(fromIdents a []), (fromIdents b [])]
  }
}; */

let _parseLongCap children => {
  let leafs = RU.getChildren children (fun (_, child) => {
    switch child {
      | Leaf ("capIdent", _) contents _ => Some contents
      | _ => None
    }
  });
  /* let rec loop leafs => switch leafs {
    | [contents] => Lident contents
    | [contents, ...rest] => Ldot (loop rest) contents
    | _ => failwith "invalid longcap"
  }; */
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
  /* loop leafs */
};

let rec parseLongCap_ (sub, children, _) => {
  switch sub {
    | "lident" => Lident (RU.getContentsByType children "capIdent" |> unwrap)
    | "dot" => Ldot (RU.getNodeByType children "longCap_" |> unwrap |> parseLongCap_) (RU.getContentsByType children "capIdent" |> unwrap)
    | _ => failwith ("Invalid longCap_ sub " ^ sub)
  }
};

let parseLongCap (_, children, _) => {
  RU.getNodeByType children "longCap_" |> unwrap |> parseLongCap_
};

let parseLongIdent (_, children, _) => {
  let first = RU.getNodeByType children "longCap_" |> optMap parseLongCap_;
  let last = RU.getContentsByType children "lowerIdent" |> unwrap;
  switch first {
    | Some x => Ldot x last
    | None => Lident last
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

let rec fromLongCap_ longident => {
  switch longident {
    /* TODO lower vs caps */
    | Lident x => Node ("longCap_", "lident") [("", Leaf ("capIdent", "") x mLoc)] mLoc
    | Ldot a b => Node ("longCap_", "dot") [("", fromLongCap_ a), ("", Leaf ("capIdent", "") b mLoc)] mLoc
    | Lapply a b => failwith "long cap can't have an lapply"
  }
};

let fromLongIdent longident => {
  let children = switch longident {
    | Lident contents => [("", Leaf ("lowerIdent", "") contents mLoc)]
    | Ldot a b => [("", fromLongCap_ a), ("", Leaf ("lowerIdent", "") b mLoc)]
    | _ => failwith "invalid longident"
  };
  Node ("longIdent", "") children mLoc;
};

let fromLongCap longident => {
  Node ("longCap", "") [("", fromLongCap_ longident)] mLoc
};

let nodeWrap rulename (sub, children, loc) => Node (rulename, sub) children loc;
let escapeString text => "\"" ^ (String.escaped text) ^ "\"";

let parseConstant (sub, children, loc) => {
  let contents = RU.getContentsByLabel children "val" |> unwrap;
  switch sub {
    | "int" => Const_int (int_of_string contents)
    | "string" => Const_string (processString contents) None /* TODO multiline string */
    | "float" => Const_float contents
    | "char" => Const_char (String.get (processString contents) 0)
    | _ => failwith "nop"
  }
};

let fromConstant constant => {
  switch constant {
    | Const_int value => ("int", [("val", Leaf ("int64", "") (string_of_int value) mLoc)], mLoc)
    | Const_string text multi => ("string", [("val", Leaf ("string", "") (escapeString text) mLoc)], mLoc)
    | Const_float text => ("float", [("val", Leaf ("float", "") text mLoc)], mLoc)
    | Const_char chr => ("char", [("val", Leaf ("char", "") (Printf.sprintf "'%c'" chr) mLoc)], mLoc)
    | _ => failwith "unsup const"
  }
};

let ocamlLoc loc => Location.none;

let emptyLabeled fn x => ("", fn x);
let withEmptyLabels x => ("", x);

let labeled label fn x => (label, fn x);

let rec listToConstruct list maybeRest typeC tupleC => {
  switch list {
    | [] => {
      switch maybeRest {
        | None => typeC (Location.mkloc (Lident "[]") loc) None
        | Some x => x
      }
    }
    | [one, ...rest] => typeC (Location.mkloc (Lident "::") loc) (Some (tupleC [one, (listToConstruct rest maybeRest typeC tupleC)]))
  }
};

let rec listFromConstruct ({ppat_desc, _} as pattern) => {
  switch ppat_desc {
    | Ppat_tuple [first, second] => {
      [("", (fromPattern first)), ...(listFromConstruct second)]
    }
    | Ppat_var {txt, _} => [("rest", Leaf ("lowerIdent", "") txt mLoc)]
    | Ppat_construct {txt: Lident "[]", _} _ => []
    | Ppat_construct {txt: Lident "::", _} (Some pattern) => listFromConstruct pattern
    | _ => [("", fromPattern pattern)]
  }
} and

fromPattern {ppat_desc, _} => {
  let (sub, children) = switch ppat_desc {
    | Ppat_var {txt, _} => ("ident", [("", Leaf ("lowerIdent", "") txt mLoc)])
    | Ppat_tuple items => {
      ("tuple", (List.map (emptyLabeled fromPattern) items))
    }
    | Ppat_construct {txt: Lident "[]", _} _ => ("list", [])
    | Ppat_construct {txt: Lident "::", _} (Some pattern) => ("list", (listFromConstruct pattern))
    | Ppat_any => ("ignore", [])
    | Ppat_constant constant => ("const", [("", fromConstant constant |> nodeWrap "constant")])
    | Ppat_construct lid maybeArg => ("constructor", [
      ("", fromLongCap lid.txt),
      ...(switch maybeArg {
        | None => []
        | Some {ppat_desc: Ppat_tuple items, _} => List.map (emptyLabeled fromPattern) items
        | Some pat => [("", fromPattern pat)]
      })
    ])
    | _ => failwith "nop pat"
  };
  Node ("Pattern", sub) children mLoc
};

let rec parsePattern toOcaml (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  switch sub {
    | "ident" => {
      let name = RU.getContentsByType children "lowerIdent" |> unwrap;
      H.Pat.var (Location.mkloc name oloc)
    }
    | "tuple" => {
      H.Pat.tuple (RU.getNodesByType children "Pattern" (parsePattern toOcaml))
    }
    | "list" => {
      listToConstruct
      (RU.getNodesByType children "Pattern" (parsePattern toOcaml))
      ((RU.getContentsByLabel children "rest") |> optFlatMap (fun label => Some (H.Pat.var (Location.mkloc label oloc))))
      H.Pat.construct H.Pat.tuple
      /* H.Pat.construct  */
      /* H.Pat.list (RU.getContentsByType children "Pattern" (parsePattern toOcaml)) */
    }
    | "const" => H.Pat.constant (RU.getNodeByType children "constant" |> unwrap |> parseConstant)
    | "ignore" => H.Pat.any ()
    | "constructor" => H.Pat.construct
    (RU.getNodeByType children "longCap" |> unwrap |> parseLongCap |> Location.mknoloc)
    (switch (RU.getNodesByType children "Pattern" (parsePattern toOcaml)) {
      | [] => None
      | [arg] => Some arg
      | args => Some (H.Pat.tuple args)
    })
    | _ => failwith ("not impl pattern stuff" ^ sub)
  }
};

let parseModuleDesc toOcaml (sub, children, loc) => {
  /* RU.getChildren children (convertStructures toOcaml) */
  switch sub {
    | "structure" => {
      Pmod_structure (RU.getNodesByType children "Structure" (toOcaml.structure toOcaml));
    }
    | "ident" => {
      let ident = RU.getNodeByLabel children "ident" |> unwrap |> parseLongCap;
      Pmod_ident (Location.mkloc ident (ocamlLoc loc))
    }
    | _ => failwith "not impl"
  }
};

let fromModuleDesc fromOcaml desc => {
  switch desc {
    | Pmod_structure items => Node ("ModuleDesc", "structure") (List.map (emptyLabeled (fromOcaml.fromStructure fromOcaml)) items) mLoc
    | Pmod_ident {txt, _} => Node ("ModuleDesc", "ident") [("ident", fromLongCap txt)] mLoc
    | _ => failwith "module desc not imprt"
  }
};

let parseTypeDeclaration result => {
  failwith "failfail";
  /* switch result {
    | {typ: Nonlexical (_, "ident", _) _, children: [ident], _} => {
      {
        ptype_name: Location.mkloc (getContents ident) loc,
        ptype_params: [], /* TODO */
        ptype_cstrs: [], /* TODO */
        ptype_kind: Ptype_abstract,
        ptype_private: Public,
        ptype_manifest: None,
        ptype_attributes: [],
        ptype_loc: loc,
      }
    }
    | _ => failwith "Unsupported"
  } */
};

let parseArgValue toOcaml (sub, children, loc) => {
  switch sub {
    | "none" => None
    | "expr" => Some (RU.getNodeByType children "Expression" |> unwrap |> (toOcaml.expression toOcaml))
    | _ => failwith "unexpected argv value"
  }
};

let fromArgValue fromOcaml maybeDefault => {
  let (sub, children) = switch maybeDefault {
    | None => ("none", [])
    | Some x => ("expr", [("", fromOcaml.fromExpression fromOcaml x)])
  };
  Node ("ArgValue", sub) children mLoc;
};

let parseArg toOcaml (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  switch sub {
    | "punned" => {
      let name = RU.getContentsByType children "lowerIdent" |> unwrap;
      let maybeArg = RU.getNodeByType children "ArgValue";
      let maybeExpr = maybeArg |> optFlatMap (parseArgValue toOcaml);
      let name = maybeArg == None ? name : "?" ^ name;
      (name, H.Pat.var (Location.mkloc name oloc), maybeExpr)
    }
    | "anon" => {
      ("", RU.getNodeByType children "Pattern" |> unwrap |> parsePattern toOcaml, None)
    }
    | "named" => {
      let name = RU.getContentsByType children "lowerIdent" |> unwrap;
      let pat = RU.getNodeByType children "Pattern" |> unwrap |> parsePattern toOcaml;
      let maybeArg = RU.getNodeByType children "ArgValue";
      let maybeExpr = maybeArg |> optFlatMap (parseArgValue toOcaml);
      let name = maybeArg == None ? name : "?" ^ name;
      (name, pat, maybeExpr)
    }
    | _ => failwith "nop arg"
  }
};

let fromArg fromOcaml (label, maybeDefault, pattern) => {
  let (sub, children) = if (label == "") {
    ("anon", [("", fromPattern pattern)])
  } else {
    let ll = String.length label;
    let (label, opt) = if (ll > 0 && String.get label 0 == '?') {
      (String.sub label 1 (ll - 1), true)
    } else {
      (label, false)
    };
    let argValue = opt ? [("", fromArgValue fromOcaml maybeDefault)] : [];
    let ident = Leaf ("lowerIdent", "") label mLoc;
    switch pattern.ppat_desc {
      | Ppat_var {txt, _} when txt == label => ("punned", [("", ident), ...argValue])
      | _ => ("named", [("", ident), ("", (fromPattern pattern)), ...argValue])
    }
  };
  Node ("Arg", sub) children mLoc;
};

let makeFunction toOcaml args expr => {
  List.fold_left
  (fun expr (label, pat, maybeExpr) => {
    H.Exp.fun_ label maybeExpr pat expr
  })
  expr
  args;
};

let parseBinding toOcaml (sub, children, loc) => {
  let pvb_loc = ocamlLoc loc;
  switch sub {
    | "func" => {
      let name = RU.getContentsByLabel children "name" |> unwrap;
      let args = RU.getNodesByType children "Arg" (parseArg toOcaml);
      let expr = RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml;
      let pvb_expr = makeFunction toOcaml args expr;
      {pvb_pat: H.Pat.var (Location.mkloc name pvb_loc), pvb_expr, pvb_attributes: [], pvb_loc, }
    }
    | "value" => {
      let pvb_pat = RU.getNodeByType children "Pattern" |> unwrap |> parsePattern toOcaml;
      let pvb_expr = RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml;
      {pvb_pat, pvb_expr, pvb_attributes: [], pvb_loc}
    }
    | _ => failwith "unknown binding"
  }
};

let fromValueBinding fromOcaml {pvb_pat, pvb_expr, _} => {
  Node ("ValueBinding", "value") [("", fromPattern pvb_pat), ("", fromOcaml.fromExpression fromOcaml pvb_expr)] mLoc
  /** TODO check to see if it's a function, and if so, use the "func" subtype **/
};

let rec parseType (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  switch sub {
    | "constructor" => {
      let ident = RU.getNodeByType children "longIdent" |> unwrap |> parseLongIdent;
      let types = RU.getNodesByType children "Type" parseType;
      H.Typ.constr (Location.mkloc ident oloc) types
    }
    | _ => failwith "not support atm type"
  }
};

let parseStructure toOcaml (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  switch sub {
    | "let_module" => H.Str.module_ {
      pmb_name: (Location.mkloc (RU.getContentsByType children "capIdent" |> unwrap) oloc),
      pmb_attributes: [],
      pmb_loc: oloc,
      pmb_expr: {
        pmod_desc: RU.getNodeByType children "ModuleDesc" |> unwrap |> parseModuleDesc toOcaml,
        pmod_loc: oloc,
        pmod_attributes: [],
      }
    }
    | "value" => {
      let isRec = RU.getPresenceByLabel children "rec";
      let bindings = RU.getNodesByType children "ValueBinding" (parseBinding toOcaml);
      /* print_endline (PackTypes.Result.show_result (Node ("Structure", sub) children loc)); */
      H.Str.value (isRec ? Recursive : Nonrecursive) bindings
    }
    | "eval" => {
      H.Str.eval (RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml);
    }
    | "type" => {
      H.Str.type_ (RU.getNodesByType children "TypeDeclaration" (parseTypeDeclaration toOcaml))
    }
    | "open" => H.Str.open_ {
      popen_lid: (Location.mkloc (RU.getNodeByType children "longCap" |> unwrap |> parseLongCap) oloc),
      popen_override: Fresh,
      popen_loc: oloc,
      popen_attributes: [],
    }
    | _ => failwith ("Unknown structure type - " ^ sub)
  }
};

let fromStructure fromOcaml structure => {
  switch (structure.pstr_desc) {
    | Pstr_value recFlag valueBindings => {
      let children = (List.map (emptyLabeled (fromValueBinding fromOcaml)) valueBindings);
      let children = recFlag == Recursive ? [("rec", mLeaf), ...children] : children;
      Node ("Structure", "value") children mLoc
    }
    | Pstr_eval expr attrs => {
      Node ("Structure", "eval") [("", fromOcaml.fromExpression fromOcaml expr)] mLoc
    }
    | Pstr_module {pmb_name: {txt, _}, pmb_expr: {pmod_desc, _}, _} => {
      Node ("Structure", "let_module")
      [("", Leaf ("capIdent", "") txt mLoc), ("", fromModuleDesc fromOcaml pmod_desc)]
      mLoc
    }
    | Pstr_open {popen_lid, _} => {
      Node ("Structure", "open") [("", fromLongCap popen_lid.txt)] mLoc
    }
    /* TODO let_module, type */
    | _ => failwith "no parse structure"
  }
};

let parseFnArg toOcaml (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  switch sub {
    | "punned" => {
      let name = RU.getContentsByType children "lowerIdent" |> unwrap;
      (name, H.Exp.ident (Location.mkloc (Lident name) oloc))
    }
    | "named" => {
      let name = RU.getContentsByType children "lowerIdent" |> unwrap;
      let value = RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml;
      (name, value)
    }
    | "anon" => {
      let value = RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml;
      ("", value)
    }
    | _ => failwith ("unknown fnarg type " ^ sub)
  }
};

let fromFnArg fromOcaml (label, arg) => {
  switch arg.pexp_desc {
    | Pexp_ident {txt: Lident name, _} when name == label => {
      Node ("FnArg", "punned") [("", Leaf ("lowerIdent", "") name mLoc)] mLoc
    }
    | _ => {
      let exp = fromOcaml.fromExpression fromOcaml arg;
      if (label == "") {
        Node ("FnArg", "anon") [("", exp)] mLoc
      } else {
        Node ("FnArg", "named") [("", Leaf ("lowerIdent", "") label mLoc), ("", exp)] mLoc
      }
    }
  }
};

let rec unrollFunExpr label maybeDefault pattern expr => {
  let arg = (label, maybeDefault, pattern);
  switch expr.pexp_desc {
    | Pexp_fun l m p e => {
      let (rest, exp) = (unrollFunExpr l m p e);
      ([arg, ...rest], exp)
    }
    | _ => ([arg], expr)
  }
};

let fromFunExpr fromOcaml label maybeDefault pattern expr => {
  let (args, exp) = unrollFunExpr label maybeDefault pattern expr;
  let sub = switch args {
    | [one] => "single"
    | _ => "multi"
  };
  Node ("FunExpr", sub) (List.concat [(List.map (emptyLabeled (fromArg fromOcaml)) args), [("", fromOcaml.fromExpression fromOcaml exp)]]) mLoc
};

let parseFunExpr toOcaml (sub, children, loc) => {
  let args = RU.getNodesByType children "Arg" (parseArg toOcaml);
  let expr = RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml;
  makeFunction toOcaml args expr;
};

let unwrapm message opt => {
  switch opt {
    | Some x => x
    | None => raise (RU.ConversionFailure ("Unwrapping none " ^ message))
  }
};

let fromLet fromOcaml isRec values => {
  let bindings = List.map (emptyLabeled (fromValueBinding fromOcaml)) values;
  Node ("Statement", "value")
  (isRec == Recursive ? [("rec", Leaf ("", "") "rec" mLoc), ...bindings] : bindings)
  mLoc
};

let rec unwrapSequence fromOcaml exp => {
  switch exp.pexp_desc {
    | Pexp_sequence first second => {
      List.concat [unwrapSequence fromOcaml first, unwrapSequence fromOcaml second]
      /* [fromOcaml.fromExpression fromOcaml first, ...unwrapSequence fromOcaml second] */
    }
    | Pexp_let isRec values exp => {
      [fromLet fromOcaml isRec values, ...unwrapSequence fromOcaml exp]
    }
    | Pexp_letmodule {txt, _} modexp exp => {
      failwith "letmodule not yet"
    }
    | _ => [Node ("Statement", "expr") [("", fromOcaml.fromExpression fromOcaml exp)] mLoc]
  }
};

let stripRuleName ((name, sub), children, loc) => (sub, children, loc);

let stringToIdentLoc loc txt => Location.mkloc (Lident txt) loc;

let parseBlock toOcaml (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  /* let exprs = RU.getNodesByType children "Expression" (toOcaml.expression toOcaml); */
  let rec loop children => {
    switch children {
      | [] => H.Exp.ident (Location.mkloc (Lident "()") oloc)
      | [(_, Leaf _), ...rest] => loop rest
      | [(_, Node ("Statement", "expr") children _)] => getExpression toOcaml children
      | [(_, Node ("Statement", "expr") children _), ...rest] => H.Exp.sequence (getExpression toOcaml children) (loop rest)
      | [(_, Node ("Statement", "value") children _), ...rest] => {
        let isRec = RU.getPresenceByLabel children "rec";
        let bindings = RU.getNodesByType children "ValueBinding" (parseBinding toOcaml);
        H.Exp.let_ (isRec ? Recursive : Nonrecursive) bindings (loop rest)
      }
      | [(_, Node ("Statement", "module") children _), ...rest] => {
        let name = (Location.mkloc (RU.getContentsByType children "capIdent" |> unwrap) oloc);
        H.Exp.letmodule name {
          pmod_desc: RU.getNodeByType children "ModuleDesc" |> unwrap |> parseModuleDesc toOcaml,
          pmod_loc: oloc,
          pmod_attributes: [],
        } (loop rest)
      }
      | _ => failwith "Unknown statement"
    }
  };
  loop children
};

let fromBlock fromOcaml expr => {
  /* XXX duplication from fromExpression :/ */
  switch (expr.pexp_desc) {
    | Pexp_let isRec values exp => {
      let children = [fromLet fromOcaml isRec values, ...unwrapSequence fromOcaml exp];
      Node ("Block", "") (List.map withEmptyLabels children) mLoc
    }
    | Pexp_sequence first second => {
      let children = List.concat [unwrapSequence fromOcaml first, unwrapSequence fromOcaml second];
      Node ("Block", "") (List.map withEmptyLabels children) mLoc
    }
    | _ => {
      let node = fromOcaml.fromExpression fromOcaml expr;
      Node ("Block", "") [("", Node ("Statement", "expr") [("", node)] mLoc)] mLoc;
    }
  }
};

/** SWITCH **/
let parseSwitchCase toOcaml (sub, children, loc) => {
  let pattern = RU.getNodeByType children "Pattern" |> unwrap |> parsePattern toOcaml;
  let guard = RU.getNodeByLabel children "guard" |> optMap stripRuleName |> optMap (toOcaml.expression toOcaml);
  let body = RU.getNodeByLabel children "body" |> unwrap |> stripRuleName |> toOcaml.expression toOcaml;
  {pc_lhs: pattern, pc_guard: guard, pc_rhs: body}
};

let parseSwitchExp toOcaml (sub, children, loc) => {
  let base = RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml;
  let cases = RU.getNodesByType children "SwitchCase" (parseSwitchCase toOcaml);
  H.Exp.match_ base cases
};

let fromSwitchCase fromOcaml {pc_lhs, pc_guard, pc_rhs} => {
  let pattern = fromPattern pc_lhs;
  let guard = pc_guard |> optMap (fromOcaml.fromExpression fromOcaml);
  let body = pc_rhs |> fromOcaml.fromExpression fromOcaml;
  let children = switch guard {
    | None => [("", pattern), ("body", body)]
    | Some guard => [("", pattern), ("guard", guard), ("body", body)]
  };
  Node ("SwitchCase", "") children mLoc;
};

let fromSwitchExp fromOcaml base cases => {
  let base = fromOcaml.fromExpression fromOcaml base;
  let cases = List.map (fromSwitchCase fromOcaml) cases;
  ("switch", [("", Node ("SwitchExp", "") [("", base), ...List.map withEmptyLabels cases] mLoc)])
};
/** END SWITCH **/

/** IF **/
let parseIfExp toOcaml (sub, children, loc) => {
  let conditions = RU.getNodesByType children "Expression" (toOcaml.expression toOcaml);
  let consequents = RU.getNodesByType children "Block" (parseBlock toOcaml);
  let rec loop exprs blocks => {
    switch (exprs, blocks) {
      | ([], [elseblock]) => Some elseblock
      | ([], []) => None
      | ([expr, ...exprs], [block, ...blocks]) => {
        Some (H.Exp.ifthenelse expr block (loop exprs blocks))
      }
      | _ => failwith "Invalid ifthenelse"
    }
  };
  loop conditions consequents |> unwrap
};

let fromIfExp fromOcaml (cond, cons, maybeAlt) => {
  let rec loop ({pexp_desc, _} as expression) => {
    switch pexp_desc {
      | Pexp_ifthenelse cond cons maybeAlt => {
        switch maybeAlt {
          | None => ([cond], [cons])
          | Some alt => {
            let (exps, blocks) = loop alt;
            ([cond, ...exps], [cons, ...blocks])
          }
        }
      }
      | _ => ([], [expression])
    }
  };
  let (conds, blocks) = switch maybeAlt {
    | Some alternate => loop alternate;
    | None => ([], [])
  };
  let conds = [cond, ...conds] |> List.map (fromOcaml.fromExpression fromOcaml);
  let blocks = [cons, ...blocks] |> List.map (fromBlock fromOcaml);
  let children = List.map withEmptyLabels (List.concat [conds, blocks]);
  ("if", [("", Node ("IfExpr", "") children mLoc)])
};
/** END IF **/

let parseTry toOcaml (_, children, loc) => {
  let block = RU.getNodeByType children "Block" |> unwrap |> parseBlock toOcaml;
  let cases = RU.getNodesByType children "SwitchCase" (parseSwitchCase toOcaml);
  H.Exp.try_ block cases
};

let fromTry fromOcaml block cases => {
  let block = block |> fromBlock fromOcaml;
  let cases = List.map (emptyLabeled (fromSwitchCase fromOcaml)) cases;
  ("try", [("", Node ("TryExp", "") [("", block), ...cases] mLoc)])
};

let parseConstructor toOcaml children => {
  let lid = RU.getNodeByType children "longCap" |> unwrap |> parseLongCap |> Location.mknoloc;
  let args = RU.getNodesByType children "Expression" (toOcaml.expression toOcaml);
  let arg = switch args {
    | [] => None
    | [arg] => Some arg
    | _ => Some (H.Exp.tuple args)
  };
  H.Exp.construct lid arg;
};

let fromConstructor fromOcaml txt maybeValue => {
  let first = ("", fromLongCap txt);
  let children = switch maybeValue {
    | None => [first]
    | Some {pexp_desc: Pexp_tuple items} => {
      [first, ...List.map (emptyLabeled (fromOcaml.fromExpression fromOcaml)) items]
    }
    | Some x => [first, ("", fromOcaml.fromExpression fromOcaml x)]
  };
  ("constructor", children)
};

let parseRecord toOcaml children => {
  let extends = RU.getNodeByType children "Expression" |> optMap (toOcaml.expression toOcaml);
  let items = RU.getNodesByType children "RecordItem" (fun (sub, children, loc) => {
    let oloc = ocamlLoc loc;
    let name = RU.getNodeByType children "longIdent" |> unwrapm "long ident record" |> parseLongIdent;
    let expr = RU.getNodeByType children "Expression" |> optMap (toOcaml.expression toOcaml);
    let expr = switch expr {
      | Some x => x
      | None => H.Exp.ident (Location.mkloc name oloc)
    };
    ((Location.mkloc name oloc), expr)
  });
  H.Exp.record items extends
};

let rec parseBaseExpression toOcaml (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  switch sub {
    | "wrapped" => {
      let expr = RU.getNodeByType children "Expression" |> unwrap |> (toOcaml.expression toOcaml);
      switch (RU.getNodeByType children "Type") {
        | None => expr
        | Some x => H.Exp.constraint_ expr (parseType x)
      }
    }
    | "unexp" => {
      let ident = RU.getContentsByType children "unOp" |> unwrap |> stringToIdentLoc oloc |> H.Exp.ident;
      let main = RU.getNodeByType children "BaseExpression" |> unwrap |> parseBaseExpression toOcaml;
      /* let main = getExpression toOcaml children; */
      H.Exp.apply ident [("", main)];
    }
    | "binop" => {
      RU.getContentsByType children "binOp" |> unwrapm "binOp" |> stringToIdentLoc oloc |> H.Exp.ident;
    }
    | "ident" => {
      let ident = RU.getNodeByType children "longIdent" |> unwrap |> parseLongIdent;
      H.Exp.ident (Location.mkloc ident oloc)
    }
    | "application" => {
      let base = RU.getNodeByType children "Expression" |> unwrap |> toOcaml.expression toOcaml;
      let args = RU.getNodesByType children "FnArg" (parseFnArg toOcaml);
      H.Exp.apply base args;
    }
    | "const" => H.Exp.constant (RU.getNodeByType children "constant" |> unwrap |> parseConstant)
    | "tuple" => H.Exp.tuple (RU.getNodesByType children "Expression" (toOcaml.expression toOcaml))
    | "funexpr" => RU.getNodeByType children "FunExpr" |> unwrap |> parseFunExpr toOcaml;
    | "block" => RU.getNodeByType children "Block" |> unwrap |> parseBlock toOcaml;
    | "try" => RU.getNodeByType children "TryExp" |> unwrap |> parseTry toOcaml;
    | "constructor" => parseConstructor toOcaml children
    | "record" => parseRecord toOcaml children
    | "if" => RU.getNodeByType children "IfExpr" |> unwrap |> parseIfExp toOcaml;
    | "switch" => RU.getNodeByType children "SwitchExp" |> unwrap |> parseSwitchExp toOcaml;
    | "get_attr" => {
      H.Exp.field (RU.getNodeByType children "BaseExpression" |> unwrap |> parseBaseExpression toOcaml) (Location.mkloc (RU.getNodeByType children "longIdent" |> unwrap |> parseLongIdent) oloc)
    }
    | "unit" => H.Exp.construct (Location.mknoloc (Lident "()")) None
    | "list" => H.Exp.array (RU.getNodesByType children "Expression" (toOcaml.expression toOcaml))
    | _ => failwith ("not impl - expression - " ^ sub)
  }
};

let parseBinExpression toOcaml (sub, children, loc) => {
  switch sub {
    | "base" => RU.getNodeByType children "BaseExpression" |> unwrap |> parseBaseExpression toOcaml
    | "binexp" => {
      let oloc = ocamlLoc loc;
      let ops = RU.getChildren children (fun (label, node) => {
        switch (label, node) {
          | ("op", Leaf _ contents _) => Some (contents |> stringToIdentLoc oloc)
          | _ => None
        }
      });
      let items = RU.getNodesByType children "BaseExpression" (parseBaseExpression toOcaml);
      switch items {
        | [] => failwith "no binexp items"
        | [exp, ...rest] => {
          let rec loop ops items coll => {
            switch (ops, items) {
              | ([], []) => coll
              | ([op, ...ops], [item, ...items]) => loop ops items (H.Exp.apply (H.Exp.ident op) [("", coll), ("", item)])
              | _ => failwith "uneven binops"
            }
          };
          loop ops rest exp
        }
      }
    }
    | _ => parseBaseExpression toOcaml (sub, children, loc)
    /* | _ => failwith ("unknown expressio type " ^ sub) */
  }
};

let parseExpression toOcaml (sub, children, loc) => {
  switch sub {
    | "base" => RU.getNodeByType children "BaseExpression" |> unwrap |> parseBaseExpression toOcaml
    | "binary" => RU.getNodeByType children "BinExpression" |> unwrap |> parseBinExpression toOcaml
    | "ternary" => {
      let condition = RU.getNodeByLabel children "condition" |> unwrapm "condition" |> stripRuleName |> parseBinExpression toOcaml;
      let consequent = RU.getNodeByLabel children "consequent" |> unwrapm "consequent" |> stripRuleName |> parseBinExpression toOcaml;
      let alternate = RU.getNodeByLabel children "alternate" |> unwrapm "alternate" |> stripRuleName |> parseBinExpression toOcaml;
      H.Exp.ifthenelse attrs::[(Location.mknoloc "ternary", PStr [])] condition consequent (Some alternate)
    }
    | _ => parseBinExpression toOcaml (sub, children, loc)
    /* | _ => failwith ("unknown expressio type " ^ sub) */
  }
};

let rec unwrapList fromOcaml ({pexp_desc, _} as expression) => {
  switch pexp_desc {
    | Pexp_construct {txt: Lident "[]", _} None => []
    | Pexp_construct {txt: Lident "::", _} (Some {pexp_desc: Pexp_tuple [first, second], _}) => {
      [fromOcaml.fromExpression fromOcaml first, ...unwrapList fromOcaml second]
    }
    | _ => [fromOcaml.fromExpression fromOcaml expression]
  }
};

let opChars = "!?~$%&*+-./:<=>@^|";
let binOpChars = "$%&*+-./:<=>@^|";
let unOpChars = "!?~";
let startsWith chars txt => String.contains chars (String.get txt 0);

let wrapBinExp (sub, children) => ("base", [("", Node ("BaseExpression", "wrapped") [("", Node ("Expression", "binary") [("", Node ("BinExpression", sub) children mLoc)] mLoc)] mLoc)]);
let wrapBaseExp (sub, children) => ("wrapped", [("", Node ("Expression", "base") [("", Node ("BaseExpression", sub) children mLoc)] mLoc)]);
let wrapExp (sub, children) => ("wrapped", [("", Node ("Expression", sub) children mLoc)]);

let fromRecord fromOcaml items extends => {
  let exp = extends |> optMap (fromOcaml.fromExpression fromOcaml);
  let args = List.map (emptyLabeled (fun (ident, exp) => {
    let children = switch (exp.pexp_desc) {
      | Pexp_ident {txt: name, _} when name == ident.txt => {
        [("", fromLongIdent ident.txt)]
      }
      | _ => [("", fromLongIdent ident.txt), ("", fromOcaml.fromExpression fromOcaml exp)]
    };
    Node ("RecordItem", "") children mLoc
  })) items;
  let children = switch exp {
    | Some x => [("", x), ...args]
    | None => args
  };
  ("record", children)
};

let rec fromBaseExpression fromOcaml ({pexp_desc, pexp_attributes, _} as expression) => {
  let (sub, children) =
  switch pexp_desc {
    | Pexp_ident {txt, _} => switch txt {
      | Lident txt when startsWith opChars txt => ("binop", [("", Leaf ("binOp", "") txt mLoc)])
      | _ => ("ident", [("", fromLongIdent txt)])
    }
    | Pexp_constant constant => ("const", [("", fromConstant constant |> nodeWrap "constant")])
    | Pexp_fun label maybeDefault pattern expr => ("funexpr", [("", fromFunExpr fromOcaml label maybeDefault pattern expr)])
    | Pexp_apply base args => {
      switch (base, args) {
        | ({pexp_desc: Pexp_ident {txt: Lident txt, _}}, [("", arg)]) when startsWith unOpChars txt || txt == "-" || txt == "-." =>
          ("unexp", [("", Leaf ("unOp", "") txt mLoc), ("", fromBaseExpression fromOcaml arg)]) |> wrapBaseExp
        | _ => ("application", [("", fromOcaml.fromExpression fromOcaml base), ...(List.map (emptyLabeled (fromFnArg fromOcaml)) args)])
      }
    }
    | Pexp_record items extends => fromRecord fromOcaml items extends
    | Pexp_let isRec values exp => {
      let children = [fromLet fromOcaml isRec values, ...unwrapSequence fromOcaml exp];
      ("block", [("", Node ("Block", "") (List.map withEmptyLabels children) mLoc)])
    }
    | Pexp_sequence first second => {
      let children = List.concat [unwrapSequence fromOcaml first, unwrapSequence fromOcaml second];
      ("block", [("", Node ("Block", "") (List.map withEmptyLabels children) mLoc)])
    }
    | Pexp_tuple items => ("tuple", (List.map (emptyLabeled (fromOcaml.fromExpression fromOcaml)) items))
    | Pexp_field expr {txt, _} => ("get_attr", [("", fromBaseExpression fromOcaml expr), ("", fromLongIdent txt)])
    | Pexp_construct {txt: Lident "[]", _} None => ("list", [])
    | Pexp_construct {txt: Lident "::", _} (Some {pexp_desc: Pexp_tuple [first, second], _}) => {
      ("list", [("", (fromOcaml.fromExpression fromOcaml first)), ...List.map withEmptyLabels (unwrapList fromOcaml second)])
    }
    | Pexp_construct {txt, _} maybeValue => fromConstructor fromOcaml txt maybeValue
    | Pexp_try base cases => fromTry fromOcaml base cases
    | Pexp_match base cases => fromSwitchExp fromOcaml base cases
    | Pexp_ifthenelse condition consequent maybeAlt => { /* ? add an attribute to indicate ternary? */
      switch (pexp_attributes, maybeAlt) {
        | ([({txt: "ternary", _}, _)], Some alternate) => ("ternary", [("condition", fromBinExpression fromOcaml condition), ("consequent", fromBinExpression fromOcaml consequent), ("alternate", fromBinExpression fromOcaml alternate)]) |> wrapExp
        | _ => fromIfExp fromOcaml (condition, consequent, maybeAlt)
      }
    }
    | _ => {
      Printast.expression 0 Format.std_formatter expression;
      failwith "no exp"
    }
  };
  Node ("BaseExpression", sub) children mLoc
}

and fromBinExp fromOcaml op left right => {
    [("", fromBaseExpression fromOcaml left),
    ("op", Leaf ("binOp", "") op mLoc),
    ("", fromBaseExpression fromOcaml right)]
}

and fromBinExpression fromOcaml ({pexp_desc, _} as expression) => {
  let (sub, children) =
  switch pexp_desc {
    | Pexp_apply {pexp_desc: Pexp_ident {txt: Lident txt, _}} args
        when startsWith binOpChars txt || txt == "or" || txt == "mod" => {
          switch args {
            | [("", left), ("", right)] => ("binexp", fromBinExp fromOcaml txt left right) |> wrapBinExp
            | _ => ("base", [("", fromBaseExpression fromOcaml expression)])
          }
        }
    | _ => {
      ("base", [("", fromBaseExpression fromOcaml expression)])
    }
  };
  Node ("BinExpression", sub) children mLoc
};

let fromExpression fromOcaml ({pexp_desc, pexp_attributes, _} as expression) => {
  let (sub, children) =
  switch (pexp_attributes, pexp_desc) {
    | ([({txt: "ternary", _}, _)], Pexp_ifthenelse condition consequent (Some alternate)) =>
        ("ternary", [("condition", fromBinExpression fromOcaml condition), ("consequent", fromBinExpression fromOcaml consequent), ("alternate", fromBinExpression fromOcaml alternate)])
    | _ => ("binary", [("", fromBinExpression fromOcaml expression)])
  };
  Node ("Expression", sub) children mLoc
};

let toOcaml = {
  structure: parseStructure,
  expression: parseExpression,
};

let fromOcaml = {
  fromStructure: fromStructure,
  fromExpression: fromExpression,
};

let convert result => {
  switch result {
    | Node ("Start", _) children _ => RU.getNodesByType children "Structure" (toOcaml.structure toOcaml)
    | _ => failwith ""
  }
};

let convertFrom structures => {
  Node ("Start", "") (List.map (labeled "" (fromOcaml.fromStructure fromOcaml)) structures) (0, 0)
};
