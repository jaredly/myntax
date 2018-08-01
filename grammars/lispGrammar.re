
open Asttypes;
open Parsetree;
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
  ("let", {|"("& "def" LetPair &")"|},
    (~loc, [@node "LetPair"]pair) => H.Str.value(~loc, Nonrecursive, [pair])
  ),
  ( "let_rec", {|"("& "def-rec" LetPair+ &")"|},
    (~loc, [@nodes "LetPair"]pairs) => H.Str.value(~loc, Recursive, pairs)
  ),
  ( "type", {|"("& "type" TypeBody &")"|},
    (~loc, [@nodes "TypePair"]pairs) => H.Str.type_(pairs),
  ),
  ( "module", {|"("& "module" capIdent ModuleExpr &")"|},
    (~loc, [@text "capIdent"](name, nameLoc), [@node "ModuleExpr"]expr) => H.Str.module_(~loc, H.Mb.mk(
      Location.mkloc(name, nameLoc),
      expr
    )),
  ),
  ( "open", {|open longCap|},
    (~loc, [@node "longCap"]lident) => H.Str.open_(~loc, H.Opn.mk(lident))
  ),
  ( "eval", "Expression",
    (~loc, [@node "Expression"]expr) => H.Str.eval(~loc, expr)
  )
]];

[@name "TypeBody"]
[%%passThroughRule "TypePair+"];

[@name "ModuleExpr"]
[%%rules [
  (
    "arrow",
    {|"("& "=>" "[" "]" Structure* &")"|},
    () => ()
  ),
  (
    "structure",
    {|"("& "str" Structure* &")"|},
    () => ()
  ),
  (
    "ident",
    {|longCap|},
    () => ()
  ),
  (
    "functor_call",
    {|"("& longCap ModuleExpr+ &")"|},
    () => ()
  )
]];

[@name "LetPair"]
[%%rule (
  {|Pattern Expressoin|},
  ([@node "Pattern"]pattern, [@node "Expression"]expr) => H.Vb.mk(pattern, expr)
)];

[@name "TypePair"]
[%%rule (
  {|TypeName TypeDecl|},
  (~loc, [@node "TypeName"](name, vbls), [@node "TypeDecl"]kind) => {
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
      Ptype_record(
        RU.getNodesByType(children, "TypeObjectItem", ((sub, children, loc)) => {
        })
      )
  ), (
    "constructors",
    {|TypeConstructor+|},
    () => ()
  ), (
    "alias",
    {|CoreType|},
    () => ()
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
        H.Typ.constr(~loc=nameLoc, Location.mkloc(Lident(name), toOcaml.toLoc(nameLoc)), [])
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
    () => ()
  ),
  (
    "variable",
    {|typeVariable|},
    () => ()
  ),
  (
    "constructor",
    {|"("& longIdent CoreType+ &")"|},
    () => ()
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
    () => ()
  ),
  (
    "js_object_attribute",
    {|"("& [attr]string [object]Expression &")"|},
    () => ()
  ),
  (
    "record_attribute",
    {|"("& attribute Expression &")"|},
    () => ()
  ),
  (
    "let",
    {|"("& "let" "["& (Pattern Expression)+ &"]" Expression+ &")"|},
    () => ()
  ),
  (
    "open",
    {|"("& "open" ModuleExpr Expression+ &")"|},
    () => ()
  ),
  (
    "open",
    {|"("& "if" [test]Expression [yes]Expression [no]Expression &")"|},
    () => ()
  ),
  (
    "module",
    {|"("& "module" capIdent ModuleExpr Expression+ &")"|},
    () => ()
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

/*



@ignoreNewlines
SwitchBody = SwitchCase+

SwitchCase = SwitchCond Expression

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
]]

ThreadItem =
| attribute -- attribute
| longIdent -- ident
| longCap -- emptyconstr
| "("& [constr]longCap [args]Expression+ &")" -- constructor
| "("& [fn]Expression [args]Expression+ &")" -- fn_call

ObjectItem =
| attribute Expression -- normal
| attribute -- punned

Arrow = "("& "=>" FnArgs Expression* &")"

FnArgs =
| lowerIdent -- single
| "()" -- unit
| "_" -- ignored
| "["& FnArgItems &"]" -- multiple

@ignoreNewlines
@passThrough
FnArgItems = FnArg+

@ignoreNewlines
FnArg =
| argLabel "as" Pattern -- destructured
| argLabel &"=?" -- optional
| argLabel &"="& Expression -- defaulted
| argLabel -- labeled
| Pattern -- unlabeled

@ignoreNewlines
Pattern =
| lowerIdent -- ident
| longCap -- empty_constr
| constant
| "()" -- unit
| "_" -- ignored
| "["& [item]Pattern* ("..."& [spread]Pattern)? &"]" -- array
| "("& "," [item]Pattern* &")" -- tuple
| "("& [constr]longCap [args]Pattern+ &")" -- constructor
| "{"& PatternObjectItem+ &"}" -- object
| "(|"& Pattern+ &")" -- or

PatternObjectItem =
| attribute Pattern -- normal
| attribute -- punned

@leaf
argLabel = '~' lowerIdent

@passThrough
Parened = "("& Expression & ")"

attribute = ':' longIdent
shortAttribute = ':' lowerIdent

longIdent = (longCap_ ".")? lowerIdent
longCap = longCap_ ~"."

longCap_ =
  | longCap_ "." capIdent -- dot
  | capIdent -- lident

constant =
  | [val]float -- float
  | [val]int64 -- int
  | [val]string -- string
  | [val]char -- char

@leaf
capIdent = ~(reserved ~identchar) 'A..Z' identchar*
@leaf
lowerIdent = ~(reserved ~identchar) 'a..z' identchar*

identchar =
  | alpha
  | digit
  | "_"

@leaf
int64 =  digit+ ~identchar

@leaf
float = digit+ '.' digit+

@leaf
string = "\"" strchar* "\""
strchar =
  | "\\" any
  | ~"\"" ~"\n" ~"\\" any

@leaf
char = "'" charchar "'"
charchar =
  | "\\" any
  | ~"'" ~"\n" ~"\\" any

reserved =
  | "fun"
  | "let"
  | "and"
  | "as"
  | "type"
  | "switch"
  | "exception"
  | "of"
  | "module"
  | "rec"
  | "open"
  | "import"
  | "try"
  | "catch"
  | "from"

alpha =
  | 'a..z'
  | 'A..Z'
digit = '0..9'

@leaf
operator = ~reservedOps opChar+ ~identchar

reservedOps =
| "=>"
| "..."

opChar =
  |"!"
  |"$"
  |"%"
  |"&"
  |"*"
  |"+"
  |"-"
  |"."
  |"/"
  ;|":"
  |"<"
  |"="
  |">"
  |"?"
  |"@"
  |"^"
  |"|"
  |"~"
 */






