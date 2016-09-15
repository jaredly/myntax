
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

let isLower x => (Char.uppercase (String.get x 0)) != (String.get x 0);

/** LEXICAL THINGS **/
let processString str => {
  str |> stripQuotes |> Scanf.unescaped
};

let optMap mapper opt => {
  switch opt {
    | Some x => mapper x
    | None => None
  }
};

let rec parseLongIdent (_, children, _) => {
  let leafs = RU.getChildren children (fun (_, child) => {
    switch child {
      | Leaf ("lowerIdent", _) contents _ => Some contents
      | Leaf ("capIdent", _) contents _ => Some contents
      | _ => None
    }
  });
  let rec loop leafs => switch leafs {
    | [contents] => Lident contents
    | [contents, ...rest] => Ldot (loop rest) contents
    | _ => failwith "invalid longcap"
  };
  loop leafs
};

let rec fromIdents longident coll => {
  switch longident {
    /* TODO lower vs caps */
    | Lident x => [("", Leaf (isLower x ? "lowerIdent" : "capIdent", "") x mLoc), ...coll]
    | Ldot a b => fromIdents a [("", Leaf (isLower b ? "lowerIdent" : "capIdent", "") b mLoc)]
    | Lapply a b => List.concat [(fromIdents a []), (fromIdents b [])]
  }
};

let fromLongIdent longident => {
  Node ("longIdent", "") (fromIdents longident []) mLoc;
};

let parseLongCap children => {
  let leafs = RU.getChildren children (fun (_, child) => {
    switch child {
      | Leaf ("capIdent", _) contents _ => Some contents
      | _ => None
    }
  });
  let rec loop leafs => switch leafs {
    | [contents] => Lident contents
    | [contents, ...rest] => Ldot (loop rest) contents
    | _ => failwith "invalid longcap"
  };
  loop leafs
};

let fromCapIdent longident => {
  Node ("capIdent", "") (fromIdents longident []) mLoc;
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
    | Const_int value => ("int", [("", Leaf ("int64", "") (string_of_int value) mLoc)], mLoc)
    | Const_string text multi => ("string", [("", Leaf ("string", "") (escapeString text) mLoc)], mLoc)
    | Const_float text => ("float", [("", Leaf ("float", "") text mLoc)], mLoc)
    | Const_char chr => ("char", [("", Leaf ("char", "") (Printf.sprintf "'%c'" chr) mLoc)], mLoc)
    | _ => failwith "unsup const"
  }
};

let ocamlLoc loc => Location.none;

let emptyLabeled fn x => ("", fn x);

let labeled label fn x => (label, fn x);

let rec fromPattern {ppat_desc, _} => {
  switch ppat_desc {
    | Ppat_var {txt, _} => Node ("Pattern", "ident") [("", Leaf ("lowerIdent", "") txt mLoc)] mLoc
    | Ppat_tuple items => {
      Node ("Pattern", "tuple") (List.map (emptyLabeled fromPattern) items) mLoc
    }
    | _ => failwith "nop pat"
  }
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
    /* | "list" => {
      H.Pat.list (RU.getContentsByType children "Pattern" (parsePattern toOcaml))
    } */
    | _ => failwith "not impl pattern stuff"
  }

  /* switch (result) {
    | {typ: Nonlexical (_, "ident", _) _, children: [{typ: Lexical _ contents _, _}], _} => {
      H.Pat.var (Location.mkloc contents loc)
    }
    | {typ: Nonlexical (_, "tuple", _) _, children: children, _} => {
      let subs = List.map parsePattern (getChildrenByType children "Pattern");
      H.Pat.tuple subs
    }
    | _ => failwith "not impl pat"
  } */
};

/* let rec parsePattern result => {
  switch (result) {
    | {typ: Nonlexical (_, "ident", _) _, children: [{typ: Lexical _ contents _, _}], _} => {
      H.Pat.var (Location.mkloc contents loc)
    }
    | {typ: Nonlexical (_, "tuple", _) _, children: children, _} => {
      let subs = List.map parsePattern (getChildrenByType children "Pattern");
      H.Pat.tuple subs
    }
    | _ => failwith "not impl pat"
  }
};

let rec parseType {typ, children, _} => {
  switch typ {
    | Nonlexical (_, "constructor", _) _ => {
      let ident = getChildByType children "longident" |> unwrap |> parseLongIdent;
      let types = getChildrenByType children "Type";
      H.Typ.constr (Location.mkloc ident loc) (List.map parseType types)
    }
    | _ => failwith "not support atm type"
  }
};

let parseExpression toOcaml result => {
  switch (result) {
    | {typ: Nonlexical (_, "wrapped", _) _, children, _} => {
      let expr = getChildByType children "Expression" |> unwrap |> (toOcaml.expression toOcaml);
      switch (getChildByType children "Type") {
        | None => expr
        | Some x => H.Exp.constraint_ expr (parseType x)
      }
    }
    | {typ: Nonlexical (_, "ident", _) _, children, _} => {
      H.Exp.ident (Location.mkloc (parseLongIdent (unwrap (getChildByType children "longident"))) loc)
    }
    | {typ: Nonlexical (_, "const", _) _, children: [{typ: constant, _}], _} => {
      H.Exp.constant (parseConstant constant)
    }
    | {typ: Nonlexical (_, "tuple", _) _, children: children, _} => {
      let children = List.map (toOcaml.expression toOcaml) (getChildrenByType children "Expression");
      H.Exp.tuple children
    }
    | _ => failwith ("not impl" ^ (PackTypes.show_result result))
  }
};

let parseArgValue toOcaml name maybeArgValue => {
  switch maybeArgValue  {
    | None => (name, None)
    | Some {typ: Nonlexical (_, "none", _) _, _} => ("?" ^ name, None)
    | Some {typ: Nonlexical (_, "expr", _) _, children} =>
    ("?" ^ name, Some (parseExpression toOcaml (unwrap (getChildByType children "Expression"))))
    | _ => failwith "Invalid ArgValue"
  }
}; */

/* let makeFunction toOcaml args expr => {
  List.fold_left
  (fun expr arg => {
    switch arg {
      | {typ: Nonlexical (_, "anon", _) _, children, _} => {
        H.Exp.fun_ "" None (parsePattern (unwrap (getChildByType children "Pattern"))) expr
      }
      | {typ: Nonlexical (_, "punned", _) _, children, _} => {
        let name = getChildByType children "ident" |> unwrap |> getContents;
        let pat = H.Pat.var (Location.mkloc name loc);
        let (name, value) = parseArgValue toOcaml name (getChildByType children "ArgValue");
        H.Exp.fun_ name value pat expr
      }
      | {typ: Nonlexical (_, "named", _) _, children, _} => {
        let name = getChildByType children "ident" |> unwrap |> getContents;
        let pat = getChildByType children "Pattern" |> unwrap |> parsePattern;
        let (name, value) = parseArgValue toOcaml name (getChildByType children "ArgValue");
        H.Exp.fun_ name value pat expr
      }
      | _ => failwith "unso"
    }
  })
  (parseExpression toOcaml expr)
  args
}; */

let makeFunction toOcaml args expr => {
  List.fold_left
  (fun expr (label, pat, maybeExpr) => {
    H.Exp.fun_ label maybeExpr pat expr
  })
  expr
  args;

/*   List.fold_left
  (fun expr arg => {
    switch arg {
      | {typ: Nonlexical (_, "anon", _) _, children, _} => {
        H.Exp.fun_ "" None (parsePattern (unwrap (getChildByType children "Pattern"))) expr
      }
      | {typ: Nonlexical (_, "punned", _) _, children, _} => {
        let name = getChildByType children "ident" |> unwrap |> getContents;
        let pat = H.Pat.var (Location.mkloc name loc);
        let (name, value) = parseArgValue toOcaml name (getChildByType children "ArgValue");
        H.Exp.fun_ name value pat expr
      }
      | {typ: Nonlexical (_, "named", _) _, children, _} => {
        let name = getChildByType children "ident" |> unwrap |> getContents;
        let pat = getChildByType children "Pattern" |> unwrap |> parsePattern;
        let (name, value) = parseArgValue toOcaml name (getChildByType children "ArgValue");
        H.Exp.fun_ name value pat expr
      }
      | _ => failwith "unso"
    }
  })
  (parseExpression toOcaml expr)
  args
   */
};

/* let parseBinding toOcaml result => {
  switch (result.typ) {
    | Nonlexical (_, "func", _) _ => {
      let name = getChild result.children "name" |> unwrap |> getContents;
      let args = getChildrenByType result.children "Arg";
      let expr = getChildByType result.children "Expression" |> unwrap;
      {
        pvb_pat: H.Pat.var (Location.mkloc name loc),
        pvb_expr: makeFunction toOcaml args expr,
        pvb_attributes: [],
        pvb_loc: loc,
      }
    }
    | Nonlexical (_, "value", _) _ => {
      let pattern = ResultUtils.getChildByType result.children "Pattern" |> unwrap;
      let expr = getChildByType result.children "Expression" |> unwrap;
      {
        pvb_pat: parsePattern pattern,
        pvb_expr: parseExpression toOcaml expr,
        pvb_attributes: [],
        pvb_loc: loc,
      }
    }
    | _ => failwith "unreachable"
  }
}; */

let parseModuleDesc toOcaml (sub, children, loc) => {
  /* RU.getChildren children (convertStructures toOcaml) */
  switch sub {
    | "structure" => {
      Pmod_structure (RU.getNodesByType children "Structure" (toOcaml.structure toOcaml));
    }
    | "ident" => {
      let (_, children, loc) = RU.getNodeByLabel children "ident" |> unwrap;
      Pmod_ident (Location.mkloc (parseLongCap children) (ocamlLoc loc))
    }
    | _ => failwith "not impl"
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

let parseArg toOcaml (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  switch sub {
    | "punned" => {
      let name = RU.getContentsByType children "lowerIdent" |> unwrap;
      let maybeExpr = RU.getNodeByType children "ArgValue" |> optMap (parseArgValue toOcaml);
      (name, H.Pat.var (Location.mkloc name oloc), maybeExpr)
    }
    | _ => failwith "nop arg"
  }
};

let fromValueBinding fromOcaml {pvb_pat, pvb_expr, _} => {
  Node ("ValueBinding", "value") [("", fromPattern pvb_pat), ("", fromOcaml.fromExpression fromOcaml pvb_expr)] mLoc
  /* switch (pvb_pat.ppat_desc) {
    | Ppat_var {txt, _} => Node ("ValueBinding", "value") [ ] mLoc
    | _ => failwith "nop vb"
  } */
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

/* let parseStructure toOcaml child => {
  switch child {
    | Node ("Structure", "value") children _ => {
    /* | Nonlexical ("Structure", "value", _) _ => { */
      let isRec = isPresent (ResultUtils.getChild children "rec");
      let bindings = ResultUtils.getChildrenByType children "ValueBinding";
      H.Str.value (isRec ? Recursive : Nonrecursive)
      (List.map (parseBinding toOcaml) bindings)
    }
    /* TODO let rec module */
    | Nonlexical ("Structure", "let_module", _) _ => {
      let name = getChildByType children "capIdent" |> unwrap |> ResultUtils.getContents;
      /* H.Str.value (isRec ? Recursive : Nonrecursive) (List.map parseBinding bindings) */
      H.Str.module_ {
        pmb_name: (Location.mkloc name loc),
        pmb_attributes: [],
        pmb_loc: loc,
        pmb_expr: {
          pmod_desc: parseModuleDesc toOcaml (unwrap (getChildByType children "ModuleDesc")),
          pmod_loc: loc,
          pmod_attributes: [],
        }
      };
    }
    | Nonlexical (_, "type", _) _ => {
      let declarations = getChildrenByType children "TypeDeclaration";
      /* TODO why not work? */
      H.Str.type_ (List.map parseTypeDeclaration declarations)
    }
    | _ => failwith "nop"
  }
}; */

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
    | "type" => {
      H.Str.type_ (RU.getNodesByType children "TypeDeclaration" (parseTypeDeclaration toOcaml))
    }
    | _ => failwith "Unknown structure type"
  }
};

let fromStructure fromOcaml structure => {
  switch (structure.pstr_desc) {
    | Pstr_value recFlag valueBindings => {
      let children = (List.map (emptyLabeled (fromValueBinding fromOcaml)) valueBindings);
      let children = recFlag == Recursive ? [("rec", mLeaf), ...children] : children;
      Node ("Structure", "value") children mLoc
    }
    | _ => failwith "no parse structure"
  }
};

let parseExpression toOcaml (sub, children, loc) => {
  let oloc = ocamlLoc loc;
  switch sub {
    | "wrapped" => {
      let expr = RU.getNodeByType children "Expression" |> unwrap |> (toOcaml.expression toOcaml);
      switch (RU.getNodeByType children "Type") {
        | None => expr
        | Some x => H.Exp.constraint_ expr (parseType x)
      }
    }
    | "ident" => {
      let ident = RU.getNodeByType children "longIdent" |> unwrap |> parseLongIdent;
      H.Exp.ident (Location.mkloc ident oloc)
    }
    | "const" => {
      H.Exp.constant (RU.getNodeByType children "constant" |> unwrap |> parseConstant)
    }
    | "tuple" => {
      let children = RU.getNodesByType children "Expression" (toOcaml.expression toOcaml);
      /* let children = List.map (toOcaml.expression toOcaml) (getChildrenByType children "Expression"); */
      H.Exp.tuple children
    }
    | _ => failwith ("not impl - expression - " ^ sub)
  }
};

let fromExpression fromOcaml {pexp_desc, _} => {
  switch pexp_desc {
    | Pexp_ident {txt, _} => Node ("Expression", "ident") [("", fromLongIdent txt)] mLoc
    | Pexp_constant constant => Node ("Expression", "const") [("", fromConstant constant |> nodeWrap "constant")] mLoc
    | _ => failwith "no exp"
  }
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
    | Node ("Start", _) children _ => RU.getNodesByLabel children "structure" (toOcaml.structure toOcaml)
    | _ => failwith ""
  }
};

let convertFrom structures => {
  Node ("Start", "") (List.map (labeled "structure" (fromOcaml.fromStructure fromOcaml)) structures) (0, 0)
};
