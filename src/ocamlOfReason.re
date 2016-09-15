
/* ResultUtils */
open Parsetree;
open PackTypes.Result;
open Longident;
open Location;
open Lexing;
open Asttypes;
let module H = Ast_helper;
let unwrap = ResultUtils.unwrap;
let getChildByType = ResultUtils.getChildByType;
let getChildrenByType = ResultUtils.getChildrenByType;
let getChild = ResultUtils.getChild;
let getChildren = ResultUtils.getChildren;
let getContents = ResultUtils.getContents;

let loc = !H.default_loc;
let str = H.Str.eval (H.Exp.array []);

let isPresent opt => switch opt { | Some _ => true | None => false };

let stripQuotes str => {
  String.sub str 1 (String.length str - 2)
};

/** LEXICAL THINGS **/
let processString str => {
  str |> stripQuotes |> Scanf.unescaped
};

let parseConstant constant => {
  switch (constant) {
    | Lexical (_, "int", _) contents _ => Const_int (int_of_string contents)
    | Lexical (_, "string", _) contents _ => Const_string (processString contents) None /* TODO multiline string */
    | Lexical (_, "float", _) contents _ => Const_float contents
    | Lexical (_, "char", _) contents _ => Const_char (String.get (processString contents) 0)
    | _ => failwith "nop"
  }
};

let parseLongCap longCap => {
  let children = getChildrenByType longCap.children "capIdent";
  let rec loop children =>
  switch children {
    | [{typ: Lexical _ contents _}] => Lident contents
    | [{typ: Lexical _ contents _}, ...rest] => Ldot (loop rest) contents
    | _ => failwith "invalid longcap"
  };
  loop children
};

let parseLongIdent ident => {
  let children = getChildrenByType ident.children "capIdent";
  let last = getChildByType ident.children "lowerIdent" |> unwrap |> getContents;
  let rec loop children =>
  switch children {
    | [{typ: Lexical _ contents _}] => Lident contents
    | [{typ: Lexical _ contents _}, ...rest] => Ldot (loop rest) contents
    | _ => failwith "invalid longcap"
  };
  switch children {
    | [] => Lident last
    | _ => Ldot (loop children) last
  }
};

type toOcaml = {
  expression: toOcaml => result => expression,
  structure: toOcaml => result => structure_item,
  /* pattern: converter => result => pattern_desc, */
  /* type_: converter => result => core_type, */
  /* expression: converter => result => expression_desc, */
  /* argValue: converter => result => (string, option expression_desc), */
  /* binding: converter => result => value_binding, */
};

let rec parsePattern result => {
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
};

let makeFunction toOcaml args expr => {
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
};

let parseBinding toOcaml result => {
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
};

let parseModuleDesc toOcaml result => {
  switch result {
    | {typ: Nonlexical (_, "structure", _) _, children, _} => {
      Pmod_structure (List.map (toOcaml.structure toOcaml) (getChildrenByType children "Structure"))
    }
    | {typ: Nonlexical (_, "ident", _) _, children: [longCap], _} => {
      Pmod_ident (Location.mkloc (parseLongCap longCap) loc)
    }
    | _ => failwith "not impl"
  }
};

let parseTypeDeclaration result => {
  switch result {
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
  }
};

let parseStructure toOcaml {typ, children, _} => {
  switch typ {
    | Nonlexical ("Structure", "value", _) _ => {
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
};

let toOcaml = {
  structure: parseStructure,
  expression: parseExpression,
};

let convert result => {
  (List.map (parseStructure toOcaml) (ResultUtils.getChildrenByType result.children "Structure"));
};
