/* ResultUtils */
open Parsetree;

open PackTypes.Result;

open Longident;

open Location;

open Lexing;

open Asttypes;

module H = Ast_helper;

module RU = ResultUtils;

let unwrap = ResultUtils.unwrap;

let loc = H.default_loc^;

let str = H.Str.eval(H.Exp.array([]));

type fromOcaml = {
  fromStructure: (fromOcaml, structure_item) => result,
  fromExpression: (fromOcaml, expression) => result
};

type loc = PackTypes.Result.loc;
type toOcaml = {
  expression: (toOcaml, (string, list((string, result)), loc, option(PackTypes.Result.comments))) => expression,
  structure: (toOcaml, (string, list((string, result)), loc, option(PackTypes.Result.comments))) => structure_item
};

let node = (a, b, c) => Node(a, b, c, None);

let optOr = (orr, opt) =>
  switch opt {
  | None => orr
  | Some(x) => x
  };

let stripRuleName = (((name, sub), children, loc, comments)) => (sub, children, loc, comments);

let mLoc = Location.none;

let mLeaf = Leaf(("", ""), "", mLoc);

let isPresent = (opt) =>
  switch opt {
  | Some(_) => true
  | None => false
  };

let stripQuotes = (str) => String.sub(str, 1, String.length(str) - 2);

let isUpper = (x) => {
  let n = Char.code(x.[0]);
  90 >= n && n >= 65
};


/*** LEXICAL THINGS **/
let processString = (str) => str |> stripQuotes |> Scanf.unescaped;

let optFlatMap = (mapper, opt) =>
  switch opt {
  | Some(x) => mapper(x)
  | None => None
  };

let optMap = (mapper, opt) =>
  switch opt {
  | Some(x) => Some(mapper(x))
  | None => None
  };

let getExpression = (toOcaml, children) =>
  RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);

/* let rec fromIdents longident coll => {
     switch longident {
       /* TODO lower vs caps */
       | Lident x => [("", Leaf (isUpper x ? "capIdent" : "lowerIdent", "") x mLoc), ...coll]
       | Ldot a b => fromIdents a [("", Leaf (isUpper b ? "capIdent" : "lowerIdent", "") b mLoc)]
       | Lapply a b => List.concat [(fromIdents a []), (fromIdents b [])]
     }
   }; */
let _parseLongCap = (children) => {
  let leafs =
    RU.getChildren(
      children,
      ((_, child)) =>
        switch child {
        | Leaf(("capIdent", _), contents, _) => Some(contents)
        | _ => None
        }
    );
  /* let rec loop leafs => switch leafs {
       | [contents] => Lident contents
       | [contents, ...rest] => Ldot (loop rest) contents
       | _ => failwith "invalid longcap"
     }; */
  let rec loop = (leafs, current) =>
    switch leafs {
    | [contents, ...rest] => loop(rest, Ldot(current, contents))
    | [] => current
    };
  switch leafs {
  | [leftMost, ...rest] => loop(rest, Lident(leftMost))
  | [] => failwith("empty longident")
  }
  /* loop leafs */
};

let rec parseLongCap_ = ((sub, children, _, _)) =>
  switch sub {
  | "lident" => Lident(RU.getContentsByType(children, "capIdent") |> unwrap)
  | "dot" =>
    Ldot(
      RU.getNodeByType(children, "longCap_") |> unwrap |> parseLongCap_,
      RU.getContentsByType(children, "capIdent") |> unwrap
    )
  | _ => failwith("Invalid longCap_ sub " ++ sub)
  };

let parseLongCap = ((_, children, _, _)) =>
  RU.getNodeByType(children, "longCap_") |> unwrap |> parseLongCap_;

let parseLongIdent = ((_, children, _, _)) => {
  let first = RU.getNodeByType(children, "longCap_") |> optMap(parseLongCap_);
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

let rec fromLongCap_ = (longident) =>
  switch longident {
  /* TODO lower vs caps */
  | Lident(x) => node(("longCap_", "lident"), [("", Leaf(("capIdent", ""), x, mLoc))], mLoc)
  | Ldot(a, b) =>
    node(("longCap_", "dot"), [("", fromLongCap_(a)), ("", Leaf(("capIdent", ""), b, mLoc))], mLoc)
  | Lapply(a, b) => failwith("long cap can't have an lapply")
  };

let fromLongIdent = (longident) => {
  let children =
    switch longident {
    | Lident(contents) => [("", Leaf(("lowerIdent", ""), contents, mLoc))]
    | Ldot(a, b) => [("", fromLongCap_(a)), ("", Leaf(("lowerIdent", ""), b, mLoc))]
    | _ => failwith("invalid longident")
    };
  node(("longIdent", ""), children, mLoc)
};

let fromLongCap = (longident) => node(("longCap", ""), [("", fromLongCap_(longident))], mLoc);

let nodeWrap = (rulename, (sub, children, loc)) => node((rulename, sub), children, loc);

let escapeString = (text) => "\"" ++ (String.escaped(text) ++ "\"");

let parseConstant = ((sub, children, loc, _comments)) => {
  let contents = RU.getContentsByLabel(children, "val") |> unwrap;
  switch sub {
  | "int" => Const_int(int_of_string(contents))
  | "string" => Const_string(processString(contents), None) /* TODO multiline string */
  | "float" => Const_float(contents)
  | "char" => Const_char(processString(contents).[0])
  | _ => failwith("nop")
  }
};

let fromConstant = (constant) =>
  switch constant {
  | Const_int(value) => ("int", [("val", Leaf(("int64", ""), string_of_int(value), mLoc))], mLoc)
  | Const_string(text, multi) => (
      "string",
      [("val", Leaf(("string", ""), escapeString(text), mLoc))],
      mLoc
    )
  | Const_float(text) => ("float", [("val", Leaf(("float", ""), text, mLoc))], mLoc)
  | Const_char(chr) => (
      "char",
      [("val", Leaf(("char", ""), Printf.sprintf("'%c'", chr), mLoc))],
      mLoc
    )
  | _ => failwith("unsup const")
  };

let ocamlLoc = (loc) => Location.none;

let emptyLabeled = (fn, x) => ("", fn(x));

let withEmptyLabels = (x) => ("", x);

let labeled = (label, fn, x) => (label, fn(x));

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

let fromPatternRecordItem = (fromPattern, ({txt: longident, _}, pattern)) => {
  let lident = fromLongIdent(longident);
  let children =
    switch pattern.ppat_desc {
    | Ppat_any => [("", lident)]
    | _ => [("", lident), ("", fromPattern(pattern))]
    };
  node(("PatternRecordItem", ""), children, mLoc)
};

let rec listFromConstruct = ({ppat_desc, _} as pattern) =>
  switch ppat_desc {
  | Ppat_tuple([first, second]) => [("", fromPattern(first)), ...listFromConstruct(second)]
  | Ppat_var({txt, _}) => [("rest", Leaf(("lowerIdent", ""), txt, mLoc))]
  | Ppat_construct({txt: Lident("[]"), _}, _) => []
  | Ppat_construct({txt: Lident("::"), _}, Some(pattern)) => listFromConstruct(pattern)
  | _ => [("", fromPattern(pattern))]
  }
and fromPattern = ({ppat_desc, _}) => {
  let (sub, children) =
    switch ppat_desc {
    | Ppat_var({txt, _}) => ("ident", [("", Leaf(("lowerIdent", ""), txt, mLoc))])
    | Ppat_tuple(items) => ("tuple", List.map(emptyLabeled(fromPattern), items))
    | Ppat_construct({txt: Lident("[]"), _}, _) => ("list", [])
    | Ppat_construct({txt: Lident("::"), _}, Some(pattern)) => ("list", listFromConstruct(pattern))
    | Ppat_any => ("ignore", [])
    | Ppat_constant(constant) => ("const", [("", fromConstant(constant) |> nodeWrap("constant"))])
    | Ppat_construct(lid, maybeArg) => (
        "constructor",
        [
          ("", fromLongCap(lid.txt)),
          ...switch maybeArg {
             | None => []
             | Some({ppat_desc: Ppat_tuple(items), _}) =>
               List.map(emptyLabeled(fromPattern), items)
             | Some(pat) => [("", fromPattern(pat))]
             }
        ]
      )
    | Ppat_alias(sub, {txt: name, _}) => (
        "as",
        [("", fromPattern(sub)), ("", Leaf(("lowerIdent", ""), name, mLoc))]
      )
    | Ppat_interval(_) => failwith("nop pat interval")
    | Ppat_variant(_) => failwith("nop pat variant")
    | Ppat_record(items, closed) =>
      let children = List.map(emptyLabeled(fromPatternRecordItem(fromPattern)), items);
      let children =
        closed == Closed ?
          children : List.concat([children, [("open", Leaf(("", ""), "_", mLoc))]]);
      ("record", children)
    | Ppat_or(one, two) => ("or", [("", fromPattern(one)), ("", fromPattern(two))])
    | _ => failwith("nop pat")
    };
  node(("Pattern", sub), children, mLoc)
};

let rec parsePattern = (toOcaml, (sub, children, loc, _comments)) => {
  let oloc = ocamlLoc(loc);
  switch sub {
  | "ident" =>
    let name = RU.getContentsByType(children, "lowerIdent") |> unwrap;
    H.Pat.var(Location.mkloc(name, oloc))
  | "tuple" => H.Pat.tuple(RU.getNodesByType(children, "Pattern", parsePattern(toOcaml)))
  | "list" =>
    listToConstruct
      (
        RU.getNodesByType(children, "Pattern", parsePattern(toOcaml)),
        RU.getContentsByLabel(children, "rest")
        |> optFlatMap((label) => Some(H.Pat.var(Location.mkloc(label, oloc)))),
        H.Pat.construct,
        H.Pat.tuple
      )
      /* H.Pat.construct  */
      /* H.Pat.list (RU.getContentsByType children "Pattern" (parsePattern toOcaml)) */
  | "const" => H.Pat.constant(RU.getNodeByType(children, "constant") |> unwrap |> parseConstant)
  | "ignore" => H.Pat.any()
  | "constructor" =>
    H.Pat.construct(
      RU.getNodeByType(children, "longCap") |> unwrap |> parseLongCap |> Location.mknoloc,
      switch (RU.getNodesByType(children, "Pattern", parsePattern(toOcaml))) {
      | [] => None
      | [arg] => Some(arg)
      | args => Some(H.Pat.tuple(args))
      }
    )
  | _ => failwith("not impl pattern stuff" ++ sub)
  }
};

let parseModuleDesc = (toOcaml, (sub, children, loc, _comments)) =>
  /* RU.getChildren children (convertStructures toOcaml) */
  switch sub {
  | "structure" =>
    Pmod_structure(RU.getNodesByType(children, "Structure", toOcaml.structure(toOcaml)))
  | "ident" =>
    let ident = RU.getNodeByLabel(children, "ident") |> unwrap |> parseLongCap;
    Pmod_ident(Location.mkloc(ident, ocamlLoc(loc)))
  | _ => failwith("not impl")
  };

let fromModuleDesc = (fromOcaml, desc) =>
  switch desc {
  | Pmod_structure(items) =>
    node(
      ("ModuleDesc", "structure"),
      List.map(emptyLabeled(fromOcaml.fromStructure(fromOcaml)), items),
      mLoc
    )
  | Pmod_ident({txt, _}) => node(("ModuleDesc", "ident"), [("ident", fromLongCap(txt))], mLoc)
  | Pmod_apply(expr, arg) => failwith("nop module desc appply")
  | _ => failwith("module desc not imprt")
  };

/*

 and type_declaration = Parsetree.type_declaration =
     {
      ptype_name: string loc;
      ptype_params: (core_type * variance) list;
            (* ('a1,...'an) t; None represents  _*)
      ptype_cstrs: (core_type * core_type * locationt) list;
            (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
      ptype_kind: type_kind;
      ptype_private: private_flag;   (* = private ... *)
      ptype_manifest: core_type option;  (* = T *)
      ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
      ptype_loc: locationt;
     }
 [@@deriving yojson]

 (*
   type t                     (abstract, no manifest)
   type t = T0                (abstract, manifest=T0)
   type t = C of T | ...      (variant,  no manifest)
   type t = T0 = C of T | ... (variant,  manifest=T0)
   type t = {l: T; ...}       (record,   no manifest)
   type t = T0 = {l : T; ...} (record,   manifest=T0)
   type t = ..                (open,     no manifest)
 *)

 and type_kind = Parsetree.type_kind =
   | Ptype_abstract
   | Ptype_variant of constructor_declaration list
         (* Invariant: non-empty list *)
   | Ptype_record of label_declaration list
         (* Invariant: non-empty list *)
   | Ptype_open
 [@@deriving yojson]
   } */
/*
 and core_type = Parsetree.core_type =
     {
      ptyp_desc: core_type_desc;
      ptyp_loc: locationt;
      ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
     }
 [@@deriving yojson]

 and core_type_desc = Parsetree.core_type_desc =
   | Ptyp_any
         (*  _ *)
   | Ptyp_var of string
         (* 'a *)
   | Ptyp_arrow of label * core_type * core_type
         (* T1 -> T2       (label = "")
            ~l:T1 -> T2    (label = "l")
            ?l:T1 -> T2    (label = "?l")
          *)
   | Ptyp_tuple of core_type list
         (* T1 * ... * Tn
            Invariant: n >= 2
         *)
   | Ptyp_constr of longidentt loc * core_type list
         (* tconstr
            T tconstr
            (T1, ..., Tn) tconstr
          *)
   | Ptyp_object of (string * attributes * core_type) list * closed_flag
         (* < l1:T1; ...; ln:Tn >     (flag = Closed)
            < l1:T1; ...; ln:Tn; .. > (flag = Open)
          *)
   | Ptyp_class of longidentt loc * core_type list
         (* #tconstr
            T #tconstr
            (T1, ..., Tn) #tconstr
          *)
   | Ptyp_alias of core_type * string
         (* T as 'a *)
   | Ptyp_variant of row_field list * closed_flag * label list option
         (* [ `A|`B ]         (flag = Closed; labels = None)
            [> `A|`B ]        (flag = Open;   labels = None)
            [< `A|`B ]        (flag = Closed; labels = Some [])
            [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
          *)
   | Ptyp_poly of string list * core_type
         (* 'a1 ... 'an. T
            Can only appear in the following context:
            - As the core_type of a Ppat_constraint node corresponding
              to a constraint on a let-binding: let x : 'a1 ... 'an. T
              = e ...
            - Under Cfk_virtual for methods (not values).
            - As the core_type of a Pctf_method node.
            - As the core_type of a Pexp_poly node.
            - As the pld_type field of a label_declaration.
            - As a core_type of a Ptyp_object node.
          *)

   | Ptyp_package of package_type
         (* (module S) *)
   | Ptyp_extension of extension
         (* [%id] *)
 [@@deriving yojson]

 */
let rec fromCoreType = ({ptyp_desc: desc, _}) => {
  let (sub, children) =
    switch desc {
    | Ptyp_any => ("any", [])
    | Ptyp_var(text) => ("var", [("", Leaf(("typeVar", ""), text, mLoc))])
    | Ptyp_arrow(label, in_type, out_type) =>
      let children = [("in", fromCoreType(in_type)), ("out", fromCoreType(out_type))];
      let children =
        if (label == "") {
          children
        } else {
          let (text, opt) =
            if (label.[0] == '?') {
              (String.sub(label, 1, String.length(label) - 1), true)
            } else {
              (label, false)
            };
          let children =
            if (opt) {
              [("optional", Leaf(("", ""), "?", mLoc))]
            } else {
              []
            };
          [("", Leaf(("lowerIdent", ""), text, mLoc)), ...children]
        };
      ("fn", children)
    | Ptyp_tuple(types) => ("tuple", List.map(emptyLabeled(fromCoreType), types))
    | Ptyp_constr(ident, args) => (
        "constructor",
        [("", fromLongIdent(ident.txt)), ...List.map(emptyLabeled(fromCoreType), args)]
      )
    | Ptyp_object(_) => failwith("no object")
    | Ptyp_class(_) => failwith("no class")
    | Ptyp_alias(typ, name) => (
        "alias",
        [("", fromCoreType(typ)), ("", Leaf(("typeVar", ""), name, mLoc))]
      )
    | Ptyp_variant(_) => failwith("no variant")
    | Ptyp_poly(_) => failwith("no poly")
    | Ptyp_package(_) => failwith("no package")
    | Ptyp_extension(_) => failwith("no exptesion")
    };
  node(("Type", sub), children, mLoc)
};

let rec toCoreType = ((sub, children, loc, _comments)) =>
  switch sub {
  | "any" => H.Typ.any()
  | "var" => H.Typ.var(RU.getContentsByType(children, "typeVar") |> unwrap)
  | "fn" =>
    let isOpt = RU.getPresenceByLabel(children, "optional");
    let label = RU.getContentsByType(children, "lowerIdent") |> optOr("");
    let label = isOpt ? "?" ++ label : label;
    let in_type = RU.getNodeByLabel(children, "in") |> unwrap |> stripRuleName |> toCoreType;
    let out_type = RU.getNodeByLabel(children, "out") |> unwrap |> stripRuleName |> toCoreType;
    H.Typ.arrow(label, in_type, out_type)
  | "tuple" => H.Typ.tuple(RU.getNodesByType(children, "Type", toCoreType))
  | "constructor" =>
    H.Typ.constr(
      RU.getNodeByType(children, "longIdent") |> unwrap |> parseLongIdent |> Location.mknoloc,
      RU.getNodesByType(children, "Type", toCoreType)
    )
  | "alias" =>
    H.Typ.alias(
      RU.getNodeByType(children, "Type") |> unwrap |> toCoreType,
      RU.getContentsByType(children, "typeVar") |> unwrap
    )
  | _ => failwith("to coretype not impl " ++ sub)
  };

let fromTypeVariantItem = ({pcd_name: name, pcd_args: args}) =>
  node(
    ("TypeVariantItem", ""),
    [("", Leaf(("capIdent", ""), name.txt, mLoc)), ...List.map(emptyLabeled(fromCoreType), args)],
    mLoc
  );

let fromTypeRecordItem = ({pld_name: name, pld_type: typ}) =>
  node(
    ("TypeRecordItem", ""),
    [("", Leaf(("lowerIdent", ""), name.txt, mLoc)), ("", fromCoreType(typ))],
    mLoc
  );

let fromTypeDeclaration =
    (fromOcaml, {ptype_kind: kind, ptype_name: name, ptype_manifest: manifest, _}) => {
  let nameLeaf = Leaf(("lowerIdent", ""), name.txt, mLoc);
  let (sub, children) =
    switch kind {
    | Ptype_abstract =>
      switch manifest {
      | None => ("ident", [("", nameLeaf)])
      | Some(core_type) => ("abs_manifest", [("", nameLeaf), ("", fromCoreType(core_type))])
      }
    | Ptype_variant(constructors) =>
      let tvar =
        node(
          ("TypeDecl", "variant"),
          [
            (
              "",
              node(
                ("TypeVariant", ""),
                List.map(emptyLabeled(fromTypeVariantItem), constructors),
                mLoc
              )
            )
          ],
          mLoc
        );
      switch manifest {
      | None => ("decl", [("", nameLeaf), ("", tvar)])
      | Some(core_type) => (
          "manifested",
          [("", nameLeaf), ("", fromCoreType(core_type)), ("", tvar)]
        )
      }
    | Ptype_record(label_declarations) =>
      let trec =
        node(
          ("TypeDecl", "record"),
          List.map(emptyLabeled(fromTypeRecordItem), label_declarations),
          mLoc
        );
      switch manifest {
      | None => ("decl", [("", nameLeaf), ("", trec)])
      | Some(core_type) => (
          "manifested",
          [("", nameLeaf), ("", fromCoreType(core_type)), ("", trec)]
        )
      }
    | Ptype_open => failwith("nop open types")
    };
  node(("TypeDeclaration", sub), children, mLoc)
};

let justChildren = ((_, children, _, _)) => children;

let parseTypeVariantItem = ((sub, children, loc, _comments)) => {
  let name = RU.getContentsByType(children, "capIdent") |> unwrap;
  let args = RU.getNodesByType(children, "Type", toCoreType);
  H.Type.constructor(~args, Location.mknoloc(name))
};

let parseTypeRecordItem = ((sub, children, loc, _comments)) => {
  let name = RU.getContentsByType(children, "lowerIdent") |> unwrap;
  let typ = RU.getNodeByType(children, "Type") |> unwrap |> toCoreType;
  H.Type.field(Location.mknoloc(name), typ)
};

let parseTypeDecl = ((sub, children, loc, _comments)) =>
  switch sub {
  | "variant" =>
    let children = RU.getNodeByType(children, "TypeVariant") |> unwrap |> justChildren;
    Ptype_variant(RU.getNodesByType(children, "TypeVariantItem", parseTypeVariantItem))
  | "record" => Ptype_record(RU.getNodesByType(children, "TypeRecordItem", parseTypeRecordItem))
  | _ => failwith("invalid type decl")
  };

let parseTypeDeclaration = (toOcaml, (sub, children, loc, _comments)) => {
  let name = Location.mknoloc(RU.getContentsByType(children, "lowerIdent") |> unwrap);
  switch sub {
  | "ident" => H.Type.mk(~kind=Ptype_abstract, name)
  | "abs_manifest" =>
    H.Type.mk(
      ~kind=Ptype_abstract,
      ~manifest=RU.getNodeByType(children, "Type") |> unwrap |> toCoreType,
      name
    )
  | "decl" =>
    H.Type.mk(~kind=RU.getNodeByType(children, "TypeDecl") |> unwrap |> parseTypeDecl, name)
  | "manifested" =>
    H.Type.mk(
      ~manifest=RU.getNodeByType(children, "Type") |> unwrap |> toCoreType,
      ~kind=RU.getNodeByType(children, "TypeDecl") |> unwrap |> parseTypeDecl,
      name
    )
  | _ => failwith("nope type declaration")
  }
};

let parseArgValue = (toOcaml, (sub, children, loc, _comments)) =>
  switch sub {
  | "none" => None
  | "expr" =>
    Some(RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml))
  | _ => failwith("unexpected argv value")
  };

let fromArgValue = (fromOcaml, maybeDefault) => {
  let (sub, children) =
    switch maybeDefault {
    | None => ("none", [])
    | Some(x) => ("expr", [("", fromOcaml.fromExpression(fromOcaml, x))])
    };
  node(("ArgValue", sub), children, mLoc)
};

let parseArg = (toOcaml, (sub, children, loc, _comments)) => {
  let oloc = ocamlLoc(loc);
  switch sub {
  | "punned" =>
    let name = RU.getContentsByType(children, "lowerIdent") |> unwrap;
    let maybeArg = RU.getNodeByType(children, "ArgValue");
    let maybeExpr = maybeArg |> optFlatMap(parseArgValue(toOcaml));
    let name = maybeArg == None ? name : "?" ++ name;
    (name, H.Pat.var(Location.mkloc(name, oloc)), maybeExpr)
  | "anon" => ("", RU.getNodeByType(children, "Pattern") |> unwrap |> parsePattern(toOcaml), None)
  | "named" =>
    let name = RU.getContentsByType(children, "lowerIdent") |> unwrap;
    let pat = RU.getNodeByType(children, "Pattern") |> unwrap |> parsePattern(toOcaml);
    let maybeArg = RU.getNodeByType(children, "ArgValue");
    let maybeExpr = maybeArg |> optFlatMap(parseArgValue(toOcaml));
    let name = maybeArg == None ? name : "?" ++ name;
    (name, pat, maybeExpr)
  | _ => failwith("nop arg")
  }
};

let fromArg = (fromOcaml, (label, maybeDefault, pattern)) => {
  let (sub, children) =
    if (label == "") {
      ("anon", [("", fromPattern(pattern))])
    } else {
      let ll = String.length(label);
      let (label, opt) =
        if (ll > 0 && label.[0] == '?') {
          (String.sub(label, 1, ll - 1), true)
        } else {
          (label, false)
        };
      let argValue = opt ? [("", fromArgValue(fromOcaml, maybeDefault))] : [];
      let ident = Leaf(("lowerIdent", ""), label, mLoc);
      switch pattern.ppat_desc {
      | Ppat_var({txt, _}) when txt == label => ("punned", [("", ident), ...argValue])
      | _ => ("named", [("", ident), ("", fromPattern(pattern)), ...argValue])
      }
    };
  node(("Arg", sub), children, mLoc)
};

let makeFunction = (toOcaml, args, expr) =>
  List.fold_left(
    (expr, (label, pat, maybeExpr)) => H.Exp.fun_(label, maybeExpr, pat, expr),
    expr,
    args
  );

let parseBinding = (toOcaml, (sub, children, loc, _comments)) => {
  let pvb_loc = ocamlLoc(loc);
  switch sub {
  | "func" =>
    let name = RU.getContentsByLabel(children, "name") |> unwrap;
    let args = RU.getNodesByType(children, "Arg", parseArg(toOcaml));
    let expr = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
    let pvb_expr = makeFunction(toOcaml, args, expr);
    {pvb_pat: H.Pat.var(Location.mkloc(name, pvb_loc)), pvb_expr, pvb_attributes: [], pvb_loc}
  | "value" =>
    let pvb_pat = RU.getNodeByType(children, "Pattern") |> unwrap |> parsePattern(toOcaml);
    let pvb_expr =
      RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
    {pvb_pat, pvb_expr, pvb_attributes: [], pvb_loc}
  | _ => failwith("unknown binding")
  }
};

let fromValueBinding = (fromOcaml, {pvb_pat, pvb_expr, _}) =>
  node
    (
      ("ValueBinding", "value"),
      [("", fromPattern(pvb_pat)), ("", fromOcaml.fromExpression(fromOcaml, pvb_expr))],
      mLoc
    );
    /*** TODO check to see if it's a function, and if so, use the "func" subtype **/

let rec parseType = ((sub, children, loc, _)) => {
  let oloc = ocamlLoc(loc);
  switch sub {
  | "constructor" =>
    let ident = RU.getNodeByType(children, "longIdent") |> unwrap |> parseLongIdent;
    let types = RU.getNodesByType(children, "Type", parseType);
    H.Typ.constr(Location.mkloc(ident, oloc), types)
  | _ => failwith("not support atm type")
  }
};

let parseStructure = (toOcaml, (sub, children, loc, _)) => {
  let oloc = ocamlLoc(loc);
  switch sub {
  | "let_module" =>
    H.Str.module_({
      pmb_name: Location.mkloc(RU.getContentsByType(children, "capIdent") |> unwrap, oloc),
      pmb_attributes: [],
      pmb_loc: oloc,
      pmb_expr: {
        pmod_desc: RU.getNodeByType(children, "ModuleDesc") |> unwrap |> parseModuleDesc(toOcaml),
        pmod_loc: oloc,
        pmod_attributes: []
      }
    })
  | "value" =>
    let isRec = RU.getPresenceByLabel(children, "rec");
    let bindings = RU.getNodesByType(children, "ValueBinding", parseBinding(toOcaml));
    /* print_endline (PackTypes.Result.show_result (Node ("Structure", sub) children loc)); */
    H.Str.value(isRec ? Recursive : Nonrecursive, bindings)
  | "eval" =>
    H.Str.eval(RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml))
  | "type" =>
    H.Str.type_(RU.getNodesByType(children, "TypeDeclaration", parseTypeDeclaration(toOcaml)))
  | "open" =>
    H.Str.open_({
      popen_lid:
        Location.mkloc(RU.getNodeByType(children, "longCap") |> unwrap |> parseLongCap, oloc),
      popen_override: Fresh,
      popen_loc: oloc,
      popen_attributes: []
    })
  | _ => failwith("Unknown structure type - " ++ sub)
  }
};

let fromStructure = (fromOcaml, structure) =>
  switch structure.pstr_desc {
  | Pstr_value(recFlag, valueBindings) =>
    let children = List.map(emptyLabeled(fromValueBinding(fromOcaml)), valueBindings);
    let children = recFlag == Recursive ? [("rec", mLeaf), ...children] : children;
    node(("Structure", "value"), children, mLoc)
  | Pstr_eval(expr, attrs) =>
    node(("Structure", "eval"), [("", fromOcaml.fromExpression(fromOcaml, expr))], mLoc)
  | Pstr_module({pmb_name: {txt, _}, pmb_expr: {pmod_desc, _}, _}) =>
    node(
      ("Structure", "let_module"),
      [("", Leaf(("capIdent", ""), txt, mLoc)), ("", fromModuleDesc(fromOcaml, pmod_desc))],
      mLoc
    )
  | Pstr_open({popen_lid, _}) =>
    node(("Structure", "open"), [("", fromLongCap(popen_lid.txt))], mLoc)
  | Pstr_type(declarations) =>
    node(
      ("Structure", "type"),
      List.map(emptyLabeled(fromTypeDeclaration(fromOcaml)), declarations),
      mLoc
    )
  | Pstr_exception({pext_name: {txt: name}, pext_kind, _}) =>
    switch pext_kind {
    | Pext_decl(args, manifest) =>
      node(
        ("Structure", "exception"),
        [("", Leaf(("capIdent", ""), name, mLoc)), ...List.map(emptyLabeled(fromCoreType), args)],
        mLoc
      )
    | Pext_rebind(_) => failwith("no ext rebind")
    }
  /* TODO let_module, type */
  | _ =>
    Printast.structure(0, Format.std_formatter, [structure]);
    failwith("no parse structure")
  };

let parseFnArg = (toOcaml, (sub, children, loc, _comments)) => {
  let oloc = ocamlLoc(loc);
  switch sub {
  | "punned" =>
    let name = RU.getContentsByType(children, "lowerIdent") |> unwrap;
    (name, H.Exp.ident(Location.mkloc(Lident(name), oloc)))
  | "named" =>
    let name = RU.getContentsByType(children, "lowerIdent") |> unwrap;
    let value = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
    (name, value)
  | "anon" =>
    let value = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
    ("", value)
  | _ => failwith("unknown fnarg type " ++ sub)
  }
};

let fromFnArg = (fromOcaml, (label, arg)) =>
  switch arg.pexp_desc {
  | Pexp_ident({txt: Lident(name), _}) when name == label =>
    node(("FnArg", "punned"), [("", Leaf(("lowerIdent", ""), name, mLoc))], mLoc)
  | _ =>
    let exp = fromOcaml.fromExpression(fromOcaml, arg);
    if (label == "") {
      node(("FnArg", "anon"), [("", exp)], mLoc)
    } else {
      node(("FnArg", "named"), [("", Leaf(("lowerIdent", ""), label, mLoc)), ("", exp)], mLoc)
    }
  };

let rec unrollFunExpr = (label, maybeDefault, pattern, expr) => {
  let arg = (label, maybeDefault, pattern);
  switch expr.pexp_desc {
  | Pexp_fun(l, m, p, e) =>
    let (rest, exp) = unrollFunExpr(l, m, p, e);
    ([arg, ...rest], exp)
  | _ => ([arg], expr)
  }
};

let fromFunExpr = (fromOcaml, label, maybeDefault, pattern, expr) => {
  let (args, exp) = unrollFunExpr(label, maybeDefault, pattern, expr);
  let sub =
    switch args {
    | [one] => "single"
    | _ => "multi"
    };
  node(
    ("FunExpr", sub),
    List.concat([
      List.map(emptyLabeled(fromArg(fromOcaml)), args),
      [("", fromOcaml.fromExpression(fromOcaml, exp))]
    ]),
    mLoc
  )
};

let parseFunExpr = (toOcaml, (sub, children, loc, _comments)) => {
  let args = RU.getNodesByType(children, "Arg", parseArg(toOcaml));
  let expr = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
  makeFunction(toOcaml, args, expr)
};

let unwrapm = (message, opt) =>
  switch opt {
  | Some(x) => x
  | None => raise(RU.ConversionFailure("Unwrapping none " ++ message))
  };

let fromLet = (fromOcaml, isRec, values) => {
  let bindings = List.map(emptyLabeled(fromValueBinding(fromOcaml)), values);
  node(
    ("Statement", "value"),
    isRec == Recursive ? [("rec", Leaf(("", ""), "rec", mLoc)), ...bindings] : bindings,
    mLoc
  )
};

let rec unwrapSequence = (fromOcaml, exp) =>
  switch exp.pexp_desc {
  | Pexp_sequence(first, second) =>
    List.concat
      ([unwrapSequence(fromOcaml, first), unwrapSequence(fromOcaml, second)])
      /* [fromOcaml.fromExpression fromOcaml first, ...unwrapSequence fromOcaml second] */
  | Pexp_let(isRec, values, exp) => [
      fromLet(fromOcaml, isRec, values),
      ...unwrapSequence(fromOcaml, exp)
    ]
  | Pexp_letmodule({txt, _}, modexp, exp) => failwith("letmodule not yet")
  | _ => [node(("Statement", "expr"), [("", fromOcaml.fromExpression(fromOcaml, exp))], mLoc)]
  };

let stringToIdentLoc = (loc, txt) => Location.mkloc(Lident(txt), loc);

let parseBlock = (toOcaml, (sub, children, loc, _comments)) => {
  let oloc = ocamlLoc(loc);
  /* let exprs = RU.getNodesByType children "Expression" (toOcaml.expression toOcaml); */
  let rec loop = (children) =>
    switch children {
    | [] => H.Exp.ident(Location.mkloc(Lident("()"), oloc))
    | [(_, Leaf(_)), ...rest] => loop(rest)
    | [(_, Node(("Statement", "expr"), children, _, _))] => getExpression(toOcaml, children)
    | [(_, Node(("Statement", "expr"), children, _, _)), ...rest] =>
      H.Exp.sequence(getExpression(toOcaml, children), loop(rest))
    | [(_, Node(("Statement", "value"), children, _, _)), ...rest] =>
      let isRec = RU.getPresenceByLabel(children, "rec");
      let bindings = RU.getNodesByType(children, "ValueBinding", parseBinding(toOcaml));
      H.Exp.let_(isRec ? Recursive : Nonrecursive, bindings, loop(rest))
    | [(_, Node(("Statement", "module"), children, _, _)), ...rest] =>
      let name = Location.mkloc(RU.getContentsByType(children, "capIdent") |> unwrap, oloc);
      H.Exp.letmodule(
        name,
        {
          pmod_desc: RU.getNodeByType(children, "ModuleDesc") |> unwrap |> parseModuleDesc(toOcaml),
          pmod_loc: oloc,
          pmod_attributes: []
        },
        loop(rest)
      )
    | _ => failwith("Unknown statement")
    };
  loop(children)
};

let fromBlock = (fromOcaml, expr) =>
  /* XXX duplication from fromExpression :/ */
  switch expr.pexp_desc {
  | Pexp_let(isRec, values, exp) =>
    let children = [fromLet(fromOcaml, isRec, values), ...unwrapSequence(fromOcaml, exp)];
    node(("Block", ""), List.map(withEmptyLabels, children), mLoc)
  | Pexp_sequence(first, second) =>
    let children =
      List.concat([unwrapSequence(fromOcaml, first), unwrapSequence(fromOcaml, second)]);
    node(("Block", ""), List.map(withEmptyLabels, children), mLoc)
  | _ =>
    let node_ = fromOcaml.fromExpression(fromOcaml, expr);
    node(("Block", ""), [("", node(("Statement", "expr"), [("", node_)], mLoc))], mLoc)
  };


/*** SWITCH **/
let parseSwitchCase = (toOcaml, (sub, children, loc, _comments)) => {
  let pattern = RU.getNodeByType(children, "Pattern") |> unwrap |> parsePattern(toOcaml);
  let guard =
    RU.getNodeByLabel(children, "guard")
    |> optMap(stripRuleName)
    |> optMap(toOcaml.expression(toOcaml));
  let body =
    RU.getNodeByLabel(children, "body") |> unwrap |> stripRuleName |> toOcaml.expression(toOcaml);
  {pc_lhs: pattern, pc_guard: guard, pc_rhs: body}
};

let parseSwitchExp = (toOcaml, (sub, children, loc, _comments)) => {
  let base = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
  let cases = RU.getNodesByType(children, "SwitchCase", parseSwitchCase(toOcaml));
  H.Exp.match(base, cases)
};

let fromSwitchCase = (fromOcaml, {pc_lhs, pc_guard, pc_rhs}) => {
  let pattern = fromPattern(pc_lhs);
  let guard = pc_guard |> optMap(fromOcaml.fromExpression(fromOcaml));
  let body = pc_rhs |> fromOcaml.fromExpression(fromOcaml);
  let children =
    switch guard {
    | None => [("", pattern), ("body", body)]
    | Some(guard) => [("", pattern), ("guard", guard), ("body", body)]
    };
  node(("SwitchCase", ""), children, mLoc)
};

let fromSwitchExp = (fromOcaml, base, cases) => {
  let base = fromOcaml.fromExpression(fromOcaml, base);
  let cases = List.map(fromSwitchCase(fromOcaml), cases);
  (
    "switch",
    [("", node(("SwitchExp", ""), [("", base), ...List.map(withEmptyLabels, cases)], mLoc))]
  )
};


/*** END SWITCH **/

/*** IF **/
let parseIfExp = (toOcaml, (sub, children, loc, _comments)) => {
  let conditions = RU.getNodesByType(children, "Expression", toOcaml.expression(toOcaml));
  let consequents = RU.getNodesByType(children, "Block", parseBlock(toOcaml));
  let rec loop = (exprs, blocks) =>
    switch (exprs, blocks) {
    | ([], [elseblock]) => Some(elseblock)
    | ([], []) => None
    | ([expr, ...exprs], [block, ...blocks]) =>
      Some(H.Exp.ifthenelse(expr, block, loop(exprs, blocks)))
    | _ => failwith("Invalid ifthenelse")
    };
  loop(conditions, consequents) |> unwrap
};

let fromIfExp = (fromOcaml, (cond, cons, maybeAlt)) => {
  let rec loop = ({pexp_desc, _} as expression) =>
    switch pexp_desc {
    | Pexp_ifthenelse(cond, cons, maybeAlt) =>
      switch maybeAlt {
      | None => ([cond], [cons])
      | Some(alt) =>
        let (exps, blocks) = loop(alt);
        ([cond, ...exps], [cons, ...blocks])
      }
    | _ => ([], [expression])
    };
  let (conds, blocks) =
    switch maybeAlt {
    | Some(alternate) => loop(alternate)
    | None => ([], [])
    };
  let conds = [cond, ...conds] |> List.map(fromOcaml.fromExpression(fromOcaml));
  let blocks = [cons, ...blocks] |> List.map(fromBlock(fromOcaml));
  let children = List.map(withEmptyLabels, List.concat([conds, blocks]));
  ("if", [("", node(("IfExpr", ""), children, mLoc))])
};


/*** END IF **/
let parseTry = (toOcaml, (_, children, loc, _)) => {
  let block = RU.getNodeByType(children, "Block") |> unwrap |> parseBlock(toOcaml);
  let cases = RU.getNodesByType(children, "SwitchCase", parseSwitchCase(toOcaml));
  H.Exp.try_(block, cases)
};

let fromTry = (fromOcaml, block, cases) => {
  let block = block |> fromBlock(fromOcaml);
  let cases = List.map(emptyLabeled(fromSwitchCase(fromOcaml)), cases);
  ("try", [("", node(("TryExp", ""), [("", block), ...cases], mLoc))])
};

let parseConstructor = (toOcaml, children) => {
  let lid = RU.getNodeByType(children, "longCap") |> unwrap |> parseLongCap |> Location.mknoloc;
  let args = RU.getNodesByType(children, "Expression", toOcaml.expression(toOcaml));
  let arg =
    switch args {
    | [] => None
    | [arg] => Some(arg)
    | _ => Some(H.Exp.tuple(args))
    };
  H.Exp.construct(lid, arg)
};

let fromConstructor = (fromOcaml, txt, maybeValue) => {
  let first = ("", fromLongCap(txt));
  let children =
    switch maybeValue {
    | None => [first]
    | Some({pexp_desc: Pexp_tuple(items)}) => [
        first,
        ...List.map(emptyLabeled(fromOcaml.fromExpression(fromOcaml)), items)
      ]
    | Some(x) => [first, ("", fromOcaml.fromExpression(fromOcaml, x))]
    };
  ("constructor", children)
};

let parseRecord = (toOcaml, children) => {
  let extends = RU.getNodeByType(children, "Expression") |> optMap(toOcaml.expression(toOcaml));
  let items =
    RU.getNodesByType(
      children,
      "RecordItem",
      ((sub, children, loc, _comments)) => {
        let oloc = ocamlLoc(loc);
        let name =
          RU.getNodeByType(children, "longIdent") |> unwrapm("long ident record") |> parseLongIdent;
        let expr = RU.getNodeByType(children, "Expression") |> optMap(toOcaml.expression(toOcaml));
        let expr =
          switch expr {
          | Some(x) => x
          | None => H.Exp.ident(Location.mkloc(name, oloc))
          };
        (Location.mkloc(name, oloc), expr)
      }
    );
  H.Exp.record(items, extends)
};

let rec parseBaseExpression = (toOcaml, (sub, children, loc, _comments)) => {
  let oloc = ocamlLoc(loc);
  switch sub {
  | "wrapped" =>
    let expr = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
    switch (RU.getNodeByType(children, "Type")) {
    | None => expr
    | Some(x) => H.Exp.constraint_(expr, parseType(x))
    }
  | "unexp" =>
    let ident =
      RU.getContentsByType(children, "unOp") |> unwrap |> stringToIdentLoc(oloc) |> H.Exp.ident;
    let main =
      RU.getNodeByType(children, "BaseExpression") |> unwrap |> parseBaseExpression(toOcaml);
    /* let main = getExpression toOcaml children; */
    H.Exp.apply(ident, [("", main)])
  | "binop" =>
    RU.getContentsByType(children, "binOp")
    |> unwrapm("binOp")
    |> stringToIdentLoc(oloc)
    |> H.Exp.ident
  | "ident" =>
    let ident = RU.getNodeByType(children, "longIdent") |> unwrap |> parseLongIdent;
    H.Exp.ident(Location.mkloc(ident, oloc))
  | "application" =>
    let base = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
    let args = RU.getNodesByType(children, "FnArg", parseFnArg(toOcaml));
    H.Exp.apply(base, args)
  | "const" => H.Exp.constant(RU.getNodeByType(children, "constant") |> unwrap |> parseConstant)
  | "tuple" => H.Exp.tuple(RU.getNodesByType(children, "Expression", toOcaml.expression(toOcaml)))
  | "funexpr" => RU.getNodeByType(children, "FunExpr") |> unwrap |> parseFunExpr(toOcaml)
  | "block" => RU.getNodeByType(children, "Block") |> unwrap |> parseBlock(toOcaml)
  | "try" => RU.getNodeByType(children, "TryExp") |> unwrap |> parseTry(toOcaml)
  | "while" =>
    let cond = RU.getNodeByType(children, "Expression") |> unwrap |> toOcaml.expression(toOcaml);
    let block = RU.getNodeByType(children, "Block") |> unwrap |> parseBlock(toOcaml);
    H.Exp.while_(cond, block)
  | "constructor" => parseConstructor(toOcaml, children)
  | "record" => parseRecord(toOcaml, children)
  | "if" => RU.getNodeByType(children, "IfExpr") |> unwrap |> parseIfExp(toOcaml)
  | "switch" => RU.getNodeByType(children, "SwitchExp") |> unwrap |> parseSwitchExp(toOcaml)
  | "get_attr" =>
    H.Exp.field(
      RU.getNodeByType(children, "BaseExpression") |> unwrap |> parseBaseExpression(toOcaml),
      Location.mkloc(RU.getNodeByType(children, "longIdent") |> unwrap |> parseLongIdent, oloc)
    )
  | "unit" => H.Exp.construct(Location.mknoloc(Lident("()")), None)
  | "list" => H.Exp.array(RU.getNodesByType(children, "Expression", toOcaml.expression(toOcaml)))
  | _ => failwith("not impl - expression - " ++ sub)
  }
};

let parseBinExpression = (toOcaml, (sub, children, loc, _comments)) =>
  switch sub {
  | "base" =>
    RU.getNodeByType(children, "BaseExpression") |> unwrap |> parseBaseExpression(toOcaml)
  | "binexp" =>
    let oloc = ocamlLoc(loc);
    let ops =
      RU.getChildren(
        children,
        ((label, node)) =>
          switch (label, node) {
          | ("op", Leaf(_, contents, _)) => Some(contents |> stringToIdentLoc(oloc))
          | _ => None
          }
      );
    let items = RU.getNodesByType(children, "BaseExpression", parseBaseExpression(toOcaml));
    switch items {
    | [] => failwith("no binexp items")
    | [exp, ...rest] =>
      let rec loop = (ops, items, coll) =>
        switch (ops, items) {
        | ([], []) => coll
        | ([op, ...ops], [item, ...items]) =>
          loop(ops, items, H.Exp.apply(H.Exp.ident(op), [("", coll), ("", item)]))
        | _ => failwith("uneven binops")
        };
      loop(ops, rest, exp)
    }
  | _ => parseBaseExpression(toOcaml, (sub, children, loc, _comments))
  /* | _ => failwith ("unknown expressio type " ^ sub) */
  };

let parseExpression = (toOcaml, (sub, children, loc, _comments)) =>
  switch sub {
  | "base" =>
    RU.getNodeByType(children, "BaseExpression") |> unwrap |> parseBaseExpression(toOcaml)
  | "binary" =>
    RU.getNodeByType(children, "BinExpression") |> unwrap |> parseBinExpression(toOcaml)
  | "ternary" =>
    let condition =
      RU.getNodeByLabel(children, "condition")
      |> unwrapm("condition")
      |> stripRuleName
      |> parseBinExpression(toOcaml);
    let consequent =
      RU.getNodeByLabel(children, "consequent")
      |> unwrapm("consequent")
      |> stripRuleName
      |> parseBinExpression(toOcaml);
    let alternate =
      RU.getNodeByLabel(children, "alternate")
      |> unwrapm("alternate")
      |> stripRuleName
      |> parseBinExpression(toOcaml);
    H.Exp.ifthenelse(
      ~attrs=[(Location.mknoloc("ternary"), PStr([]))],
      condition,
      consequent,
      Some(alternate)
    )
  | _ => parseBinExpression(toOcaml, (sub, children, loc, _comments))
  /* | _ => failwith ("unknown expressio type " ^ sub) */
  };

let rec unwrapList = (fromOcaml, {pexp_desc, _} as expression) =>
  switch pexp_desc {
  | Pexp_construct({txt: Lident("[]"), _}, None) => []
  | Pexp_construct({txt: Lident("::"), _}, Some({pexp_desc: Pexp_tuple([first, second]), _})) => [
      fromOcaml.fromExpression(fromOcaml, first),
      ...unwrapList(fromOcaml, second)
    ]
  | _ => [fromOcaml.fromExpression(fromOcaml, expression)]
  };

let opChars = "!?~$%&*+-./:<=>@^|";

let binOpChars = "$%&*+-./:<=>@^|";

let unOpChars = "!?~";

let startsWith = (chars, txt) => String.contains(chars, txt.[0]);

let wrapBinExp = ((sub, children)) => (
  "base",
  [
    (
      "",
      node(
        ("BaseExpression", "wrapped"),
        [
          (
            "",
            node(
              ("Expression", "binary"),
              [("", node(("BinExpression", sub), children, mLoc))],
              mLoc
            )
          )
        ],
        mLoc
      )
    )
  ]
);

let wrapBaseExp = ((sub, children)) => (
  "wrapped",
  [("", node(("Expression", "base"), [("", node(("BaseExpression", sub), children, mLoc))], mLoc))]
);

let wrapExp = ((sub, children)) => ("wrapped", [("", node(("Expression", sub), children, mLoc))]);

let fromRecord = (fromOcaml, items, extends) => {
  let exp = extends |> optMap(fromOcaml.fromExpression(fromOcaml));
  let args =
    List.map(
      emptyLabeled(
        ((ident, exp)) => {
          let children =
            switch exp.pexp_desc {
            | Pexp_ident({txt: name, _}) when name == ident.txt => [("", fromLongIdent(ident.txt))]
            | _ => [("", fromLongIdent(ident.txt)), ("", fromOcaml.fromExpression(fromOcaml, exp))]
            };
          node(("RecordItem", ""), children, mLoc)
        }
      ),
      items
    );
  let children =
    switch exp {
    | Some(x) => [("", x), ...args]
    | None => args
    };
  ("record", children)
};

let rec fromBaseExpression = (fromOcaml, {pexp_desc, pexp_attributes, _} as expression) => {
  let (sub, children) =
    switch pexp_desc {
    | Pexp_ident({txt, _}) =>
      switch txt {
      | Lident(txt) when startsWith(opChars, txt) => (
          "binop",
          [("", Leaf(("binOp", ""), txt, mLoc))]
        )
      | _ => ("ident", [("", fromLongIdent(txt))])
      }
    | Pexp_constant(constant) => ("const", [("", fromConstant(constant) |> nodeWrap("constant"))])
    | Pexp_fun(label, maybeDefault, pattern, expr) => (
        "funexpr",
        [("", fromFunExpr(fromOcaml, label, maybeDefault, pattern, expr))]
      )
    | Pexp_apply(base, args) =>
      switch (base, args) {
      | ({pexp_desc: Pexp_ident({txt: Lident(txt), _})}, [("", arg)])
          when startsWith(unOpChars, txt) || txt == "-" || txt == "-." =>
        ("unexp", [("", Leaf(("unOp", ""), txt, mLoc)), ("", fromBaseExpression(fromOcaml, arg))])
        |> wrapBaseExp
      | _ => (
          "application",
          [
            ("", fromOcaml.fromExpression(fromOcaml, base)),
            ...List.map(emptyLabeled(fromFnArg(fromOcaml)), args)
          ]
        )
      }
    | Pexp_record(items, extends) => fromRecord(fromOcaml, items, extends)
    | Pexp_let(isRec, values, exp) =>
      let children = [fromLet(fromOcaml, isRec, values), ...unwrapSequence(fromOcaml, exp)];
      ("block", [("", node(("Block", ""), List.map(withEmptyLabels, children), mLoc))])
    | Pexp_sequence(first, second) =>
      let children =
        List.concat([unwrapSequence(fromOcaml, first), unwrapSequence(fromOcaml, second)]);
      ("block", [("", node(("Block", ""), List.map(withEmptyLabels, children), mLoc))])
    | Pexp_tuple(items) => (
        "tuple",
        List.map(emptyLabeled(fromOcaml.fromExpression(fromOcaml)), items)
      )
    | Pexp_field(expr, {txt, _}) => (
        "get_attr",
        [("", fromBaseExpression(fromOcaml, expr)), ("", fromLongIdent(txt))]
      )
    | Pexp_construct({txt: Lident("[]"), _}, None) => ("list", [])
    | Pexp_construct({txt: Lident("::"), _}, Some({pexp_desc: Pexp_tuple([first, second]), _})) => (
        "list",
        [
          ("", fromOcaml.fromExpression(fromOcaml, first)),
          ...List.map(withEmptyLabels, unwrapList(fromOcaml, second))
        ]
      )
    | Pexp_construct({txt, _}, maybeValue) => fromConstructor(fromOcaml, txt, maybeValue)
    | Pexp_try(base, cases) => fromTry(fromOcaml, base, cases)
    | Pexp_match(base, cases) => fromSwitchExp(fromOcaml, base, cases)
    | Pexp_while(cond, block) =>
      let block = block |> fromBlock(fromOcaml);
      let cond = cond |> fromOcaml.fromExpression(fromOcaml);
      /* let cases = List.map (emptyLabeled (fromSwitchCase fromOcaml)) cases; */
      ("while", [("", cond), ("", block)])
    | Pexp_ifthenelse(condition, consequent, maybeAlt) =>
      /* ? add an attribute to indicate ternary? */
      switch (pexp_attributes, maybeAlt) {
      | ([({txt: "ternary", _}, _)], Some(alternate)) =>
        (
          "ternary",
          [
            ("condition", fromBinExpression(fromOcaml, condition)),
            ("consequent", fromBinExpression(fromOcaml, consequent)),
            ("alternate", fromBinExpression(fromOcaml, alternate))
          ]
        )
        |> wrapExp
      | _ => fromIfExp(fromOcaml, (condition, consequent, maybeAlt))
      }
    | _ =>
      Printast.expression(0, Format.std_formatter, expression);
      failwith("no exp")
    };
  node(("BaseExpression", sub), children, mLoc)
}
and fromBinExp = (fromOcaml, op, left, right) => [
  ("", fromBaseExpression(fromOcaml, left)),
  ("op", Leaf(("binOp", ""), op, mLoc)),
  ("", fromBaseExpression(fromOcaml, right))
]
and fromBinExpression = (fromOcaml, {pexp_desc, _} as expression) => {
  let (sub, children) =
    switch pexp_desc {
    | Pexp_apply({pexp_desc: Pexp_ident({txt: Lident(txt), _})}, args)
        when startsWith(binOpChars, txt) || txt == "or" || txt == "mod" =>
      switch args {
      | [("", left), ("", right)] =>
        ("binexp", fromBinExp(fromOcaml, txt, left, right)) |> wrapBinExp
      | _ => ("base", [("", fromBaseExpression(fromOcaml, expression))])
      }
    | _ => ("base", [("", fromBaseExpression(fromOcaml, expression))])
    };
  node(("BinExpression", sub), children, mLoc)
};

let fromExpression = (fromOcaml, {pexp_desc, pexp_attributes, _} as expression) => {
  let (sub, children) =
    switch (pexp_attributes, pexp_desc) {
    | ([({txt: "ternary", _}, _)], Pexp_ifthenelse(condition, consequent, Some(alternate))) => (
        "ternary",
        [
          ("condition", fromBinExpression(fromOcaml, condition)),
          ("consequent", fromBinExpression(fromOcaml, consequent)),
          ("alternate", fromBinExpression(fromOcaml, alternate))
        ]
      )
    | _ => ("binary", [("", fromBinExpression(fromOcaml, expression))])
    };
  node(("Expression", sub), children, mLoc)
};

let toOcaml = {structure: parseStructure, expression: parseExpression};

let fromOcaml = {fromStructure, fromExpression};

let convert = (result) =>
  switch result {
  | Node(("Start", _), children, _, _) =>
    RU.getNodesByType(children, "Structure", toOcaml.structure(toOcaml))
  | _ => failwith("")
  };

let convertFrom = (structures) =>
  node(
    ("Start", ""),
    List.map(labeled("", fromOcaml.fromStructure(fromOcaml)), structures),
    Location.none
  );
