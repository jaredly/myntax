
open Asttypes;
open Parsetree;

open Longident;

module H = Ast_helper;

module DSL = PackTypes.DSL;

[@lineComment ";"];
[@blockComment ("(**", "*)")];

[@name "Start"]
[%%rule (
  "ModuleBody",
  ([@nodes "Structure"]structures) => structures
)];

[@name "ModuleBody"]
[%%passThroughRule "Structure+"];

[@name "Structure"]
[%%rules [
  ("let", {|"("& "def" LetPair &")"|}, (~loc, [@node "LetPair"]pair) => H.Str.value(~loc, Nonrecursive, [pair])),
  ( "let_rec", {|"("& "def-rec" LetPair+ &")"|}, (~loc, [@nodes "LetPair"]pairs) => H.Str.value(~loc, Recursive, pairs)),
  ( "type", {|"("& "type" TypeBody &")"|}, (~loc, [@nodes "TypePair"]pairs) => H.Str.type_(pairs),),
  ( "module", {|"("& "module" capIdent ModuleExpr &")"|},
    (~loc, [@text "capIdent"](name, nameLoc), [@node "ModuleExpr"]expr) => H.Str.module_(~loc, H.Mb.mk(
      Location.mkloc(name, nameLoc),
      expr
    )),
  ),
  ( "open", {|open longCap|}, (~loc, [@node "longCap"]lident) => H.Str.open_(~loc, H.Opn.mk(lident))),
  ( "eval", "Expression", (~loc, [@node "Expression"]expr) => H.Str.eval(~loc, expr))
]];

[@name "TypeBody"]
[%%passThroughRule "TypePair+"];

[@name "ModuleExpr"]
[%%rules [
  (
    "arrow",
    {|"("& "=>" "[" "]" Structure* &")"|},
    () => failwith("not impl")
  ),
  (
    "structure",
    {|"("& "str" Structure* &")"|},
    () => failwith("not impl")
  ),
  (
    "ident",
    {|longCap|},
    () => failwith("not impl")
  ),
  (
    "functor_call",
    {|"("& longCap ModuleExpr+ &")"|},
    () => failwith("not impl")
  )
]];

[@name "LetPair"]
[%%rule (
  {|Pattern Expressoin|},
  ([@node "Pattern"]pattern, [@node "Expression"]expr) => H.Vb.mk(pattern, expr)
)];

[@name "TypePair"]
[%%rule (
  {|TypeName TypeKind|},
  (~loc, [@node "TypeName"](name, vbls), [@node "TypeKind"]kind) => {
    H.Type.mk(
      ~loc,
      ~params=vbls,
      ~kind,
      name,
    )
  },
)];

[@name "TypeName"]
[%%rules [
  (
    "vbl",
    {|"("& lowerIdent typeVariable+ &")"|},
    (~loc, [@text "lowerIdent"](name, loc), [@texts "typeVariable"]vbls) => (
      Location.mkloc(name, loc),
      vbls |> List.map(((name, loc)) => (H.Typ.var(~loc, name), Invariant)),
    )
  ),
  (
    "plain",
    {|lowerIdent|},
    (~loc, [@text "lowerIdent"](name, loc)) => (
      Location.mkloc(name, loc),
      []
    )
  )
]];

[@ignoreNewLines]
[@name "TypeKind"]
[%%rules [
  (
    "record",
    {|"{"& TypeObjectItem+ &"}"|},
    (~loc, [@nodes "TypeObjectItem"]items) =>
      Ptype_record(items)
  ), (
    "constructors",
    {|TypeConstructor+|},
    () => failwith("a")
  ), (
    "alias",
    {|CoreType|},
    () => failwith("a")
  )
]]

[@name "TypeObjectItem"]
[%%rules [
  (
    "normal",
    "shortAttribute CoreType",
    (~loc, [@node "shortAttribute"](name, nameLoc), [@node "CoreType"]t) => {
      H.Type.field(~loc, Location.mkloc(name, nameLoc), t)
    }
  ), (
    "punned",
    "shortAttribute",
    (~loc, [@node "shortAttribute"](name, nameLoc)) => {
      H.Type.field(~loc,
        Location.mkloc(name, nameLoc),
        H.Typ.constr(~loc=nameLoc, Location.mkloc(Lident(name), nameLoc), [])
      )
    }
  )
]];

[@name "shortAttribute"]
[%%rule (
  {|":" lowerIdent|},
  ([@text "lowerIdent"]pair) => pair
)];

[@name "TypeConstructor"]
[%%rules [
  (
    "no_args",
    {|longCap|},
    (~loc, [@node "longCap"]lident) => ()
  ), (
    "args",
    {|"("& longCap CoreType+ &")"|},
    (~loc, [@node "longCap"]lident, [@node "CoreType"]core) => ()
  )
]];

[@name "CoreType"]
[%%rules [
  (
    "constr_no_args",
    {|longIdent|},
    () => failwith("ct")
  ),
  (
    "variable",
    {|typeVariable|},
    () => failwith("ct")
  ),
  (
    "constructor",
    {|"("& longIdent CoreType+ &")"|},
    () => failwith("ct")
  ),
]];

[@leaf]
[@name "typeVariable"]
[%%rule {|'\'' lowerIdent|}];

[@ignoreNewlines]
[@name "Expression"]
[%%rules [
  (
    "array_index",
    {|"("& "["& [index]Expression &"]" [array]Expression &")"|},
    () => failwith("ct")
  ),
  (
    "js_object_attribute",
    {|"("& [attr]string [object]Expression &")"|},
    () => failwith("ct")
  ),
  (
    "record_attribute",
    {|"("& attribute Expression &")"|},
    () => failwith("ct")
  ),
  (
    "let",
    {|"("& "let" "["& (Pattern Expression)+ &"]" Expression+ &")"|},
    () => failwith("ct")
  ),
  (
    "open",
    {|"("& "open" ModuleExpr Expression+ &")"|},
    () => failwith("ct")
  ),
  (
    "open",
    {|"("& "if" [test]Expression [yes]Expression [no]Expression &")"|},
    () => failwith("ct")
  ),
  (
    "module",
    {|"("& "module" capIdent ModuleExpr Expression+ &")"|},
    () => failwith("ct")
  ),
    /* ; not 100% sure I want to do this :P but it could be so handy!! */
  (
    "loop_recur",
    {|"("& "loop" "["& (Pattern Expression)+ &"]" Expression+ &")"|},
    () => failwith("not impl")
  ),
  (
    "arrow",
    {|Arrow|},
    () => failwith("not impl")
  ),
  (
    "threading_last",
    {|"("& "->>" [target]Expression ThreadItem+ &")"|},
    () => failwith("not impl")
  ),
  (
    "threading",
    {|"("& "->" [target]Expression ThreadItem+ &")"|},
    () => failwith("not impl")
  ),
  (
    "switch",
    {|Switch|},
    () => failwith("not impl")
  ),
  (
    "constructor",
    {|"("& [constr]longCap [args]Expression+ &")"|},
    () => failwith("not impl")
  ),
  (
    "tuple",
    {|"("& "," [args]Expression+ &")"|},
    () => failwith("not impl")
  ),
  (
    "fn_call",
    {|"("& [fn]Expression FnCallArg+ &")"|},
    () => failwith("not impl")
  ),
  (
    "array_literal",
    {|"["& [items]Expression* ("..."& [spread]Expression)? &"]"|},
    () => failwith("not impl")
  ),
  (
    "object_literal",
    {|"{"& ("..."& [spread]Expression)? ObjectItem+ &"}"|},
    () => failwith("not impl")
  ),
  (
    "empty_constr",
    {|longCap|},
    () => failwith("not impl")
  ),
  (
    "ident",
    {|longIdent|},
    () => failwith("not impl")
  ),
  (
    "attribute",
    {|attribute|},
    () => failwith("not impl")
  ),
  (
    "op",
    {|operator|},
    () => failwith("not impl")
  ),
  (
    "const",
    {|constant|},
    () => failwith("not impl")
  ),
]];

[@name "FnCallArg"]
[%%rules [
  (
    "labeled",
    {|argLabel "=" Expression|},
    () => failwith("not impl labeled"),  ),
  (
    "punned",
    {|argLabel|},
    () => failwith("not impl punned"),
  ),
  (
    "expr",
    {|Expression|},
    () => failwith("not impl expr"),
  ),
]];

[@name "Switch"]
[%%rule {|"("& "switch" [target]Expression SwitchBody &")"|}];

[@ignoreNewlines]
[@name "SwitchBody"][%%rule "SwitchCase+"];

[@name "SwitchCase"][%%rule "SwitchCond Expression"];

[@name "SwitchCond"]
[%%rules [
  (
    "when",
    {|Pattern "when" Expression|},
    () => failwith("not impl when"),
  ),
  (
    "plain",
    {|Pattern|},
    () => failwith("not impl plain"),
  ),
]];

[@name "ThreadItem"]
[%%rules [
  (
    "attribute",
    {|attribute|},
    () => failwith("not impl attribute"),
  ),
  (
    "ident",
    {|longIdent|},
    () => failwith("not impl ident"),
  ),
  (
    "emptyconstr",
    {|longCap|},
    () => failwith("not impl emptyconstr"),
  ),
  (
    "constructor",
    {|"("& [constr]longCap [args]Expression+ &")"|},
    () => failwith("not impl constructor"),
  ),
  (
    "fn_call",
    {|"("& [fn]Expression [args]Expression+ &")"|},
    () => failwith("not impl fn_call"),
  ),
]];

[@name "ObjectItem"]
[%%rules [
  (
    "normal",
    {|attribute Expression|},
    () => failwith("not impl normal"),
  ),
  (
    "punned",
    {|attribute|},
    () => failwith("not impl punned"),
  ),
]];

[@name "Arrow"][%%rule {|"("& "=>" FnArgs Expression* &")"|}];

[@name "FnArgs"]
[%%rules [
  (
    "single",
    {|lowerIdent|},
    () => failwith("not impl single"),
  ),
  (
    "unit",
    {|"()"|},
    () => failwith("not impl unit"),
  ),
  (
    "ignored",
    {|"_"|},
    () => failwith("not impl ignored"),
  ),
  (
    "multiple",
    {|"["& FnArgItems &"]"|},
    () => failwith("not impl multiple"),
  ),
]];

[@ignoreNewlines]
[@name "FnArgItems"]
[%%passThroughRule "FnArg+"];

[@ignoreNewlines]
[@name "FnArg"]
[%%rules [
  (
    "destructured",
    {|argLabel "as" Pattern|},
    () => failwith("not impl destructured"),
  ),
  (
    "optional",
    {|argLabel &"=?"|},
    () => failwith("not impl optional"),
  ),
  (
    "defaulted",
    {|argLabel &"="& Expression|},
    () => failwith("not impl defaulted"),
  ),
  (
    "labeled",
    {|argLabel|},
    () => failwith("not impl labeled"),
  ),
  (
    "unlabeled",
    {|Pattern|},
    () => failwith("not impl unlabeled"),
  ),
]];


[@ignoreNewlines]
[@name "Pattern"]
[%%rules [
  (
    "ident",
    {|lowerIdent|},
    () => failwith("not impl ident"),
  ),
  (
    "empty_constr",
    {|longCap|},
    () => failwith("not impl empty_constr"),
  ),
  (
    "constant",
    {|constant|},
    () => failwith("not impl constant"),
  ),
  (
    "unit",
    {|"()"|},
    () => failwith("not impl unit"),
  ),
  (
    "ignored",
    {|"_"|},
    () => failwith("not impl ignored"),
  ),
  (
    "array",
    {|"["& [item]Pattern* ("..."& [spread]Pattern)? &"]"|},
    () => failwith("not impl array"),
  ),
  (
    "tuple",
    {|"("& "," [item]Pattern* &")"|},
    () => failwith("not impl tuple"),
  ),
  (
    "constructor",
    {|"("& [constr]longCap [args]Pattern+ &")"|},
    () => failwith("not impl constructor"),
  ),
  (
    "object",
    {|"{"& PatternObjectItem+ &"}"|},
    () => failwith("not impl object"),
  ),
  (
    "or",
    {|"(|"& Pattern+ &")"|},
    () => failwith("not impl or"),
  ),
]];

[@name "PatternObjectItem"]
[%%rules [
  (
    "normal",
    {|attribute Pattern|},
    () => failwith("not impl normal"),
  ),
  (
    "punned",
    {|attribute|},
    () => failwith("not impl punned"),
  ),
]];

[@leaf]
[@name "argLabel"]
[%%rule "'~' lowerIdent"];

[@passThrough]
[@name "Parened"]
[%%rule {|"("& Expression & ")"|}];

[@name "attribute"][%%rule {|':' longIdent|}];
[@name "shortAttribute"][%%rule {|':' lowerIdent|}];

[@name "longIdent"][%%rule {|(longCap_ ".")? lowerIdent|}];
[@name "longCap"][%%rule (
  {|longCap_ ~"."|},
  ([@node "longCap_"]l) => l
)];

[@name "longCap_"]
[%%rules [
  (
    "dot",
    {|longCap_ "." capIdent|},
    () => failwith("not impl dot"),
  ),
  (
    "lident",
    {|capIdent|},
    () => failwith("not impl lident"),
  ),
]];

[@name "constant"]
[%%rules [
  (
    "float",
    {|[val]float|},
    () => failwith("not impl float"),
  ),
  (
    "int",
    {|[val]int64|},
    () => failwith("not impl int"),
  ),
  (
    "string",
    {|[val]string|},
    () => failwith("not impl string"),
  ),
  (
    "char",
    {|[val]char|},
    () => failwith("not impl char"),
  ),
]];

[@leaf] [@name "capIdent"][%%rule {|~(reserved ~identchar) 'A..Z' identchar*|}];
[@leaf] [@name "lowerIdent"][%%rule {|~(reserved ~identchar) 'a..z' identchar*|}];

[@name "identchar"]
[%%rules [
  "alpha",
  "digit",
  {|"_"|},
]];


[@leaf] [@name "int64"][%%rule {|digit+ ~identchar|}];
[@leaf] [@name "float"][%%rule {|digit+ '.' digit+|}];
[@leaf] [@name "string"][%%rule {|"\"" strchar* "\""|}];

[@name "strchar"]
[%%rules [
  {|"\\" any|},
  {|~"\"" ~"\n" ~"\\" any|}
]];

[@leaf] [@name "char"][%%rule {|"'" charchar "'"|}];

[@name "charchar"]
[%%rules [
  {|"\\" any|},
  {|~"'" ~"\n" ~"\\" any|}
]];

[@name "reserved"]
[%%rules [
  {|"fun"|},
  {|"let"|},
  {|"and"|},
  {|"as"|},
  {|"type"|},
  {|"switch"|},
  {|"exception"|},
  {|"of"|},
  {|"module"|},
  {|"rec"|},
  {|"open"|},
  {|"import"|},
  {|"try"|},
  {|"catch"|},
  {|"from"|}
]];

[@name "alpha"]
[%%rules [
  {|'a..z'|},
  {|'A..Z'|},
]];

[@name "digit"][%%rule {|'0..9'|}];

[@leaf] [@name "operator"][%%rule {|~reservedOps opChar+ ~identchar|}];

[@name "reservedOps"]
[%%rules [
  {|"=>"|},
  {|"..."|},
]];

[@name "opChar"]
[%%rules [
  {|"!"|},
  {|"$"|},
  {|"%"|},
  {|"&"|},
  {|"*"|},
  {|"+"|},
  {|"-"|},
  {|"."|},
  {|"/"|},
  /* {|":"|}, */
  {|"<"|},
  {|"="|},
  {|">"|},
  {|"?"|},
  {|"@"|},
  {|"^"|},
  {|"|"|},
  {|"~"|},
]];






