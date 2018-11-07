
open Asttypes;
open Parsetree;
open Longident;

module H = Ast_helper;

let rec expressionSequence = exprs => switch exprs {
  | [] => H.Exp.construct(Location.mknoloc(Lident("()")), None)
  | [one] => one
  | [one, ...rest] => H.Exp.sequence(one, expressionSequence(rest))
};

let makeArrow = (~args, ~body) => {
  let rec loop = args => switch args {
    | [] => body
    | [(label, expr, pat), ...rest] => H.Exp.fun_(~loc=pat.ppat_loc, label, expr, pat, loop(rest))
  };
  loop(args)
};

let constructorArgs = (exprs, fn) => switch exprs {
  | [one] => one
  | _ => fn(exprs)
};

[@lineComment ";"];
[@blockComment ("(**", "*)")];

[@ignoreNewlines]
[@name "Start"]
[%%rule (
  "ModuleBody",
  ([@node "ModuleBody"]body) => body
)];

[@ignoreNewlines]
[@preserveInnerSpace]
[@name "ModuleBody"]
[%%rule ("Structure+", ([@nodes "Structure"]s) => s)];

[@preserveInnerSpace]
[@ignoreNewlines]
[@name "ExpressionSequence"]
[%%rule ("Expression*", ([@nodes "Expression"]exprs) => expressionSequence(exprs))];

/** Forms that are valid at the top level of a file or module */
[@preserveInnerSpace]
[@name "Structure"]
[%%rule [
  ( "open", {|"("& "open" > longCap &")"|}, (~loc, [@node "longCap"]lident) => H.Str.open_(~loc, H.Opn.mk(lident))),
  /** Define a toplevel value. */
  ("def", {|"("& "def"$ LetPair &")"|}, (~loc, [@node "LetPair"]pair) => H.Str.value(~loc, Nonrecursive, [pair])),
  ("defn", {|"("& "defn"$ lowerIdent$ FnArgs > ExpressionSequence &")"|}, (~loc, [@text "lowerIdent"](text, tloc), [@node "FnArgs"]args, [@node "ExpressionSequence"]body) => H.Str.value(~loc, Nonrecursive, [
    H.Vb.mk(
      ~loc,
      H.Pat.var(Location.mkloc(text, tloc)),
      makeArrow(~args, ~body)
    )
  ])),
  ("def_rec", {|"("& "def-rec"$ LetPair+ &")"|}, (~loc, [@nodes "LetPair"]pairs) => H.Str.value(~loc, Recursive, pairs)),
  ("type", {|"("& "type"$ TypeBody &")"|}, (~loc, [@nodes "TypePair"]pairs) => H.Str.type_(pairs),),
  ("module", {|"("& "module"$ capIdent > Structure+ &")"|},
    (~loc, [@text "capIdent"](name, nameLoc), [@nodes "Structure"]items) => H.Str.module_(~loc, H.Mb.mk(
      Location.mkloc(name, nameLoc),
      H.Mod.mk(~loc, Pmod_structure(items)),
    )),
  ),
  ("module_alias", {|"("& "module-alias"$ capIdent > ModuleApply &")"|},
    (~loc, [@text "capIdent"](name, nameLoc), [@node "ModuleApply"]expr) => H.Str.module_(~loc, H.Mb.mk(
      Location.mkloc(name, nameLoc),
      expr
    )),
  ),
  ( "external", {|"("& "external" lowerIdent CoreType string+ &")"|}, 
    (~loc, [@text "lowerIdent"](text, tloc), [@node "CoreType"]typ, [@texts "string"]prim) =>
      H.Str.primitive(~loc, H.Val.mk(~loc, ~prim=List.map(fst, prim) |> List.map(processString), Location.mkloc(text, tloc), typ))
  ),

  (
    "decorator_nopayload",
    {|"("& "@"& decoratorName [inner]Structure &")"|},
    (~loc, [@text "decoratorName"](text, loc), [@node.inner "Structure"]inner) => {
      let attr = (Location.mkloc(text, loc), PStr([]));
      {...inner, pstr_desc: switch (inner.pstr_desc) {
        | Pstr_primitive(vdesc) => Pstr_primitive({
          ...vdesc,
          pval_attributes: [attr, ...vdesc.pval_attributes]
        })
        | Pstr_eval(expr, attrs) => Pstr_eval(expr, [attr, ...attrs])
        | _ => failwith("Decorators only supported for expressions and `external`s")
      }}
    }
  ),

  (
    "decorator",
    {|"("& "@"& decoratorName [payload]Structure [inner]Structure &")"|},
    (~loc, [@text "decoratorName"](text, loc), [@node.payload "Structure"]payload, [@node.inner "Structure"]inner) => {
      let attr = (Location.mkloc(text, loc), PStr([payload]));
      {...inner, pstr_desc: switch (inner.pstr_desc) {
        | Pstr_primitive(vdesc) => Pstr_primitive({
          ...vdesc,
          pval_attributes: [attr, ...vdesc.pval_attributes]
        })
        | Pstr_eval(expr, attrs) => Pstr_eval(expr, [attr, ...attrs])
        | _ => failwith("Decorators only supported for expressions and `external`s")
      }}
    }
  ),

  ( "eval", "Expression", (~loc, [@node "Expression"]expr) => H.Str.eval(~loc, expr))
]];

[@preserveInnerSpace]
[@name "Expression"]
[%%rule [
  (
    "ident",
    {|longIdent|},
    (~loc, [@node "longIdent"]ident) => H.Exp.ident(~loc, ident)
  ),
  (
    "const",
    {|constant|},
    (~loc, [@node "constant"]c) => H.Exp.constant(~loc, c)
  ),
  (
    "unit",
    {|"()"|},
    (~loc) => H.Exp.construct(~loc, Location.mkloc(Lident("()"), loc), None)
  ),

  (
    "extension_expr",
    {|"("& "%"& decoratorName Structure+ &")"|},
    (~loc, [@text "decoratorName"](text, loc), [@nodes "Structure"]inner) => {
      H.Exp.extension(~loc, (Location.mkloc(text, loc), PStr(inner)))
    }
  ),

  (
    "decorator_expr_nopayload",
    {|"("& "@"& decoratorName [inner]Expression &")"|},
    (~loc, [@text "decoratorName"](text, loc), [@node.inner "Expression"]inner) => {
      let attr = (Location.mkloc(text, loc), PStr([]));
      {...inner, pexp_attributes: [attr, ...inner.pexp_attributes]}
    }
  ),

  (
    "decorator_expr",
    {|"("& "@"& decoratorName [payload]Structure [inner]Expression &")"|},
    (~loc, [@text "decoratorName"](text, loc), [@node.payload "Structure"]payload, [@node.inner "Expression"]inner) => {
      let attr = (Location.mkloc(text, loc), PStr([payload]));
      {...inner, pexp_attributes: [attr, ...inner.pexp_attributes]}
    }
  ),

  (
    "constructor",
    {|"("& longCap Expression+ &")"|},
    (~loc, [@node "longCap"]ident, [@nodes "Expression"]exprs) => H.Exp.construct(~loc, ident, Some(constructorArgs(exprs, H.Exp.tuple(~loc))))
  ),
  (
    "empty_constr",
    {|longCap|},
    (~loc, [@node "longCap"]ident) => H.Exp.construct(~loc, ident, None)
  ),
  (
    "constructor_poly",
    {|"("& polyIdent > Expression+ &")"|},
    (~loc, [@node "polyIdent"]ident, [@nodes "Expression"]exprs) => H.Exp.variant(~loc, ident.txt, Some(constructorArgs(exprs, H.Exp.tuple)))
  ),
  (
    "empty_poly",
    {|polyIdent|},
    (~loc, [@node "polyIdent"]ident) => H.Exp.variant(~loc, ident.txt, None)
  ),

  (
    "attribute",
    {|attribute|},
    (~loc, [@node "attribute"]attr) => H.Exp.fun_(~loc, "", None, H.Pat.var(Location.mkloc("x", loc)), H.Exp.field(H.Exp.ident(Location.mkloc(Lident("x"), loc)), attr))
  ),
  (
    "op",
    {|operator|},
    (~loc, [@text "operator"](op, oloc)) => H.Exp.ident(~loc, Location.mkloc(Lident(op), oloc))
  ),

  (
    "tuple",
    {|"("& "," >> Expression Expression+ &")"|},
    (~loc, [@nodes "Expression"]exprs) => H.Exp.tuple(~loc, exprs)
  ),
  (
    "array_literal",
    {|"[|"& >> [items]Expression* &"|]"|},
    (~loc, [@nodes.items "Expression"]items) => H.Exp.array(~loc, items)
  ),
  (
    "list_literal",
    {|"["& >> [items]Expression* ("..."& [spread]Expression)? &"]"|},
    (~loc, [@nodes.items "Expression"]items, [@node_opt.spread "Expression"]spread) => listToConstruct(~loc, items, spread, (~loc, a, b) => H.Exp.construct(~loc, a, b), H.Exp.tuple, item => item.pexp_loc)
  ),
  (
    "object_literal",
    {|"{"& >> ("..."& Expression)? ObjectItem* &"}"|},
    (~loc, [@node_opt "Expression"]spread, [@nodes "ObjectItem"]items) => H.Exp.record(items, spread)
  ),

  (
    "let",
    {|"("& "let"$ LetPairs > ExpressionSequence &")"|},
    (~loc, [@node "LetPairs"]bindings, [@node "ExpressionSequence"]body) =>
      H.Exp.let_(~loc, Nonrecursive, bindings, body)
  ),
  (
    "do",
    {|"("& "do" > ExpressionSequence &")"|},
    (~loc, [@node "ExpressionSequence"]body) => body
  ),
  (
    "assert",
    {|"("& "assert" > Expression &")"|},
    (~loc, [@node "Expression"]expr) => H.Exp.assert_(~loc, expr)
  ),
  (
    "lazy",
    {|"("& "lazy" > Expression &")"|},
    (~loc, [@node "Expression"]expr) => H.Exp.lazy_(~loc, expr)
  ),
  (
    "open",
    {|"("& "open"$ longCap > ExpressionSequence &")"|},
    (~loc, [@node "longCap"]ident, [@node "ExpressionSequence"]body) => H.Exp.open_(~loc, Fresh, ident, body)
  ),
  (
    "if",
    {|"("& "if"$ [test]Expression > [yes]Expression [no]Expression? &")"|},
    (~loc, [@node.test "Expression"]test, [@node.yes "Expression"]yes, [@node_opt.no "Expression"]no) => H.Exp.ifthenelse(~loc, test, yes, no)
  ),
  (
    "module_pack",
    {|"("& "module"$ ModuleExpr &")"|},
    (~loc, [@node "ModuleExpr"]modexp) => H.Exp.pack(~loc, modexp)
  ),
  (
    "module",
    {|"("& "module"$ capIdent$ ModuleExpr > ExpressionSequence &")"|},
    (~loc, [@text "capIdent"](text, tloc), [@node "ModuleExpr"]modexp, [@node "ExpressionSequence"]body) => 
    H.Exp.letmodule(~loc, Location.mkloc(text, tloc), modexp, body),
  ),
    /* ; not 100% sure I want to do this :P but it could be so handy!! */
  /* (
    "loop_recur",
    {|"("& "loop" "["& (Pattern Expression)+ &"]" Expression+ &")"|},
    () => failwith("not impl loop")
  ), */
  (
    "arrow",
    {|"("& "=>"$ FnArgs > ExpressionSequence &")"|},
    (~loc, [@node "FnArgs"]args, [@node "ExpressionSequence"]body) => {
      makeArrow(~args, ~body)
    }
  ),
  (
    "threading_last",
    {|"("& "->>"$ Expression > ThreadItem* &")"|},
    (~loc, [@node "Expression"]target, [@nodes "ThreadItem"]items) => {
      Belt.List.reduce(items, target, (target, (loc, item)) => {
        switch item {
          | `Attribute(attr) => H.Exp.field(~loc, target, attr)
          | `Fn(fn, args) => H.Exp.apply(fn, args @ [("", target)])
          | `Construct(name, args) => H.Exp.construct(name, Some(H.Exp.tuple(args @ [target])))
        }
      })
    }
  ),
  (
    "threading",
    {|"("& "->"$ [target]Expression > ThreadItem* &")"|},
    (~loc, [@node.target "Expression"]target, [@nodes "ThreadItem"]items) => {
      Belt.List.reduce(items, target, (target, (loc, item)) => {
        switch item {
          | `Attribute(attr) => H.Exp.field(~loc, target, attr)
          | `Fn(fn, args) => H.Exp.apply(fn, [("", target), ...args])
          | `Construct(name, args) => H.Exp.construct(name, Some(H.Exp.tuple([target, ...args])))
        }
      })
    }
  ),
  (
    "threading_as",
    {|"("& "as->"$ [target]Expression$ Pattern > [items]Expression* &")"|},
    (~loc, [@node.target "Expression"]target, [@node "Pattern"]pat, [@nodes.items "Expression"]items) => {
      Belt.List.reduce(items, target, (target, item) => {
        H.Exp.apply(H.Exp.fun_("", None, pat, item), [("", target)])
      })
    }
  ),
  (
    "switch",
    {|"("& "switch"$ Expression > SwitchBody &")"|},
    (~loc, [@node "Expression"]expr, [@nodes "SwitchCase"]cases) => H.Exp.match(~loc, expr, cases)
  ), (
    "switch_function",
    {|"("& "switch"$ "_" > SwitchBody &")"|},
    (~loc, [@nodes "SwitchCase"]cases) => H.Exp.function_(~loc, cases)
  ),
  (
    "try",
    {|"("& "try"$ [target]Expression > SwitchCase+ &")"|},
    (~loc, [@node "Expression"]target, [@nodes "SwitchCase"]cases) => H.Exp.try_(~loc, target, cases)
  ),


  (
    "array_index",
    {|"("& "["& [index]Expression &"]" > [array]Expression &")"|},
    (~loc, [@node.index "Expression"]index, [@node.array "Expression"]array) =>
      H.Exp.apply(~loc, H.Exp.ident(~loc, Location.mkloc(Ldot(Lident("Array"), "get"), loc)), [("", array), ("", index)])
  ),
  (
    "js_object_attribute",
    {|"("& string > Expression &")"|},
    (~loc, [@text "string"](attr, aloc), [@node "Expression"]object_) => H.Exp.apply(~loc, H.Exp.ident(~loc, Location.mkloc(Lident("##"), loc)), [("", object_), ("", H.Exp.ident(~loc=aloc, Location.mkloc(Lident(processString(attr)), aloc)))])
  ),
  (
    "setField",
    {|"("& "<-"$ attribute > [target]Expression [value]Expression &")"|},
    (~loc, [@node "attribute"]attribute, [@node.target "Expression"]target, [@node.value "Expression"]value) =>
      H.Exp.setfield(~loc, target, attribute, value)
  ),
  (
    "record_attribute",
    {|"("& attribute > Expression &")"|},
    (~loc, [@node "attribute"]attr, [@node "Expression"]expr) => H.Exp.field(~loc, expr, attr)
  ),
  (
    "fn_call",
    {|"("& Expression > FnCallArg* &")"|},
    (~loc, [@node "Expression"] fn, [@nodes "FnCallArg"] args) =>
      args == [] ?
        H.Exp.apply(~loc, fn, [("", H.Exp.construct(~loc, Location.mkloc(Lident("()"), loc), None))]) :
        H.Exp.apply(~loc, fn, args)
  ),
  (
    "constraint",
    {|"("& ":"$ Expression > CoreType &")"|},
    (~loc, [@node "Expression"]expr, [@node "CoreType"]t) => H.Exp.constraint_(~loc, expr, t)
  ),
]];

[@name "LetPairs"]
[%%rule ({| "["& >> ValueBinding+ &"]" |}, ([@nodes "ValueBinding"]bindings) => bindings)];

[@name "Pattern"]
[%%rule [
  (
    "ident", {|lowerIdent|}, (~loc, [@text "lowerIdent"](text, tloc)) => H.Pat.var(~loc, Location.mkloc(text, tloc))
  ),
  (
    "interval",
    {|[f]constant &".."& [s]constant|},
    (~loc, [@node.f "constant"]f, [@node.s "constant"]s) =>
      H.Pat.interval(~loc, f, s)
  ),
  (
    "constant",
    {|constant|},
    (~loc, [@node "constant"]const) => H.Pat.constant(~loc, const)
  ),
  ("unit", {|"()"|}, (~loc) => H.Pat.construct(~loc, Location.mkloc(Lident("()"), loc), None)),
  ("ignored", {|"_"|}, (~loc) => H.Pat.any(~loc, ())),
  (
    "array",
    {|"["& >> [items]Pattern* ("..."& [spread]Pattern)? &"]"|},
    (~loc, [@nodes.items "Pattern"]items, [@node_opt.spread "Pattern"]spread) => listToConstruct(~loc, items, spread, (~loc, a, b) => H.Pat.construct(~loc, a, b), H.Pat.tuple, item => item.ppat_loc)
  ),
  (
    "tuple",
    {|"("& "," >> Pattern Pattern+ &")"|},
    (~loc, [@nodes "Pattern"]patterns) => H.Pat.tuple(~loc, patterns)
  ),
  (
    "empty_constr", {|longCap|}, (~loc, [@node "longCap"]ident) => H.Pat.construct(~loc, ident, None)
  ),
  (
    "poly",
    {|"("& polyIdent > Pattern+ &")"|},
    (~loc, [@node "polyIdent"]ident, [@nodes "Pattern"]args) => H.Pat.variant(~loc, ident.txt, Some(constructorArgs(args, H.Pat.tuple)))
  ),
  (
    "empty_poly", {|polyIdent|}, (~loc, [@node "polyIdent"]ident) => H.Pat.variant(~loc, ident.txt, None)
  ),
  (
    "exception",
    {|"("& "exception" > Pattern &")"|},
    (~loc, [@node "Pattern"]arg) => H.Pat.exception_(arg)
  ),
  (
    "constructor",
    {|"("& longCap > Pattern+ &")"|},
    (~loc, [@node "longCap"]ident, [@nodes "Pattern"]args) => H.Pat.construct(~loc, ident, Some(constructorArgs(args, H.Pat.tuple)))
  ),
  (
    "object",
    {|"{"& >> PatternObjectItem+ &"}"|},
    (~loc, [@nodes "PatternObjectItem"]items) => H.Pat.record(~loc, items, Open)
  ),
  (
    "or",
    {|"(|" >> Pattern+ ")"|},
    (~loc, [@nodes "Pattern"]opts) => {
      let rec loop = opts => switch opts {
        | [] => assert(false)
        | [one] => one
        | [one, ...rest] => H.Pat.or_(~loc, one, loop(rest))
      };
      loop(opts)
    }
  ),
]];


[@name "TypeBody"]
[@passThrough]
[%%rule "TypePair+"];

[@name "ModuleApply"]
[%%rule [
  ("ident", {|longCap|}, (~loc, [@node "longCap"]ident) => H.Mod.mk(~loc, Pmod_ident(ident))),
  /* (
    "functor_call",
    {|"("& longCap ModuleExpr+ &")"|},
    (~loc, [@node "longCap"]ident, ) => failwith("not impl")
  ), */
]];

[@name "ModuleExpr"]
[%%rule [
  /* (
    "arrow",
    {|"("& "=>" "[" "]" > Structure* &")"|},
    () => failwith("not impl")
  ), */
  ("structure", {|"("& "str" > Structure* &")"|}, (~loc, [@nodes "Structure"]items) => H.Mod.mk(~loc, Pmod_structure(items))),
  ("ident", {|longCap|}, (~loc, [@node "longCap"]ident) => H.Mod.mk(~loc, Pmod_ident(ident))),
  /* (
    "functor_call",
    {|"("& longCap ModuleExpr+ &")"|},
    () => failwith("not impl")
  ) */
]];

[@name "LetPair"]
[%%rule (
  {|Pattern > Expression|},
  ([@node "Pattern"]pattern, [@node "Expression"]expr) => H.Vb.mk(pattern, expr)
)];

[@name "TypePair"]
[%%rule (
  {|TypeName > TypeKind|},
  (~loc, [@node "TypeName"](name, vbls), [@node "TypeKind"]kind) => {
    switch kind {
      | `Kind(kind) => H.Type.mk(~loc, ~params=vbls, ~kind, name)
      | `Manifest(manifest) => H.Type.mk(~loc, ~params=vbls, ~manifest, name)
    }
  },
)];

[@name "TypeName"]
[%%rule [
  (
    "vbl", {|"("& lowerIdent > typeVariable+ &")"|},
    (~loc, [@text "lowerIdent"](name, loc), [@texts "typeVariable"]vbls) => (
      Location.mkloc(name, loc),
      vbls |> List.map(((name, loc)) => (H.Typ.var(~loc, name), Invariant)),
    )
  ),
  (
    "plain", {|lowerIdent|}, (~loc, [@text "lowerIdent"](name, loc)) => (Location.mkloc(name, loc), [])
  )
]];

[@ignoreNewLines]
[@name "TypeKind"]
[%%rule [
  (
    "record",
    {|"{"& >> TypeObjectItem+ &"}"|},
    (~loc, [@nodes "TypeObjectItem"]items) => `Kind(Ptype_record(items))
  ), (
    "constructors",
    {|TypeConstructor+|},
    (~loc, [@nodes "TypeConstructor"]decls) => `Kind(Ptype_variant(decls))
  ), (
    "alias",
    {|CoreType|},
    ([@node "CoreType"]t) => `Manifest(t)
  )
]]

[@name "TypeObjectItem"]
[%%rule [
  (
    "normal",
    "shortAttribute > CoreType",
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
  {|":"$ lowerIdent|},
  ([@text "lowerIdent"]pair) => pair
)];

[@name "TypeConstructor"]
[%%rule [
  (
    "no_args",
    {|capIdent|},
    (~loc, [@text "capIdent"](text, tloc)) => H.Type.constructor(~loc, Location.mkloc(text, tloc))
  ), (
    "args",
    {|"("& capIdent > CoreType+ &")"|},
    (~loc, [@text "capIdent"](text, tloc), [@nodes "CoreType"]args) => H.Type.constructor(~loc, ~args, Location.mkloc(text, tloc))
  )
]];

[@name "CoreType"]
[%%rule [
  (
    "constr_no_args",
    {|longIdent|},
    (~loc, [@node "longIdent"]ident) => H.Typ.constr(~loc, ident, [])
  ),
  (
    "variable",
    {|typeVariable|},
    ([@text "typeVariable"](name, loc)) => H.Typ.var(~loc, name)
  ),
  (
    "constructor",
    {|"("& longIdent > CoreType+ &")"|},
    (~loc, [@node "longIdent"]ident, [@nodes "CoreType"]args) => H.Typ.constr(~loc, ident, args)
  ),
  (
    "arrow",
    {|"("& "=>"$ "["& [args]CoreType+ &"]" > CoreType &")"|},
    (~loc, [@node.args "CoreType"]args, [@node "CoreType"]res) => H.Typ.arrow("", args, res)
  )
]];

/** A type variable */
[@leaf]
[@name "typeVariable"]
[%%rule {|'\'' lowerIdent|}];

[@name "ValueBinding"][%%rule ("Pattern > Expression", (~loc, [@node "Pattern"]pat, [@node "Expression"]expr) => H.Vb.mk(~loc, pat, expr))]


[@name "FnCallArg"]
[%%rule [
  (
    "labeled",
    {|argLabel $&"="& > Expression|},
    ([@node "argLabel"]label, [@node "Expression"]expr) => (label.txt, expr)
  ),
  (
    "punned",
    {|argLabel|},
    ([@node "argLabel"]label) => (label.txt, H.Exp.ident(Location.mkloc(Lident(label.txt), label.loc)))
  ),
  (
    "expr",
    {|Expression|},
    ([@node "Expression"]exp) => ("", exp)
  ),
]];

[@passThrough]
[@name "SwitchBody"][%%rule "SwitchCase+"];

[@name "SwitchCase"]
[%%rule ("SwitchCond > Expression", (~loc, [@node "SwitchCond"](pat, guard), [@node "Expression"]expr) => {
  H.Exp.case(pat, ~guard?, expr)
})];

[@name "SwitchCond"]
[%%rule (
  {|Pattern ("when" Expression)?|},
  (~loc, [@node "Pattern"]pattern, [@node_opt "Expression"]guard) => (pattern, guard)
)];

[@name "ThreadItem"]
[%%rule [
  (
    "attribute",
    {|attribute|},
    (~loc, [@node "attribute"]attr) => (loc, `Attribute(attr))
  ),
  (
    "ident",
    {|longIdent|},
    (~loc, [@node "longIdent"]ident) => (loc, `Fn(H.Exp.ident(~loc, ident), []))
  ),
  (
    "emptyconstr",
    {|longCap|},
    (~loc, [@node "longCap"]ident) => (loc, `Construct(ident, []))
  ),
  (
    "constructor",
    {|"("& longCap Expression+ &")"|},
    (~loc, [@node "longCap"]ident, [@nodes "Expression"]args) => (loc, `Construct(ident, args))
  ),
  (
    "fn_call",
    {|"("& [fn]Expression [args]FnCallArg+ &")"|},
    (~loc, [@node.fn "Expression"]fn, [@nodes.args "FnCallArg"]args) => (loc, `Fn(fn, args))
  ),
]];

[@name "ObjectItem"]
[%%rule [
  (
    "normal",
    {|attribute > Expression|},
    (~loc, [@node "attribute"]attr, [@node "Expression"]expr) => (attr, expr)
  ),
  (
    "punned",
    {|attribute|},
    (~loc, [@node "attribute"]attr) => (attr, H.Exp.ident(Location.mkloc(Lident(Longident.last(attr.txt)), attr.loc)))
  ),
]];

[@name "FnArgs"]
[%%rule [
  (
    "single",
    {|lowerIdent|},
    ([@text "lowerIdent"](text, loc)) => [("", None, H.Pat.var(Location.mkloc(text, loc)))]
  ),
  (
    "unit",
    {|"()"|},
    (~loc) => [("", None, H.Pat.var(Location.mkloc("()", loc)))]
  ),
  (
    "ignored",
    {|"_"|},
    (~loc) => [("", None, H.Pat.any(~loc, ()))]
  ),
  (
    "multiple",
    {|"["& FnArgItems &"]"|},
    (~loc, [@nodes "FnArg"]args) => args
  ),
]];

[@passThrough]
[@name "FnArgItems"]
[%%rule "FnArg+"];

let argPat = (label, mtyp) => switch (mtyp) {
  | None => H.Pat.var(label)
  | Some(t) => H.Pat.constraint_(H.Pat.var(label), t)
};

[@name "FnArg"]
[%%rule [
  (
    "destructured",
    {|argLabelWithConstraint "as" Pattern|},
    (~loc, [@node "argLabelWithConstraint"](label, mtyp), [@node "Pattern"]pattern) => (label.txt, None, switch mtyp {
      | None => pattern
      | Some(mtyp) => H.Pat.constraint_(pattern, mtyp)
    })
  ),
  (
    "optional",
    {|argLabel &"=?"|},
    (~loc, [@node "argLabelWithConstraint"](label, mtyp)) => ("?" ++ label.txt, None, argPat(label, mtyp))
  ),
  (
    "defaulted",
    {|argLabelWithConstraint &"="& Expression|},
    (~loc, [@node "argLabelWithConstraint"](label, mtyp), [@node "Expression"]expr) => (label.txt, Some(expr), argPat(label, mtyp))
  ),
  (
    "labeled",
    {|argLabelWithConstraint|},
    (~loc, [@node "argLabelWithConstraint"](label, mtyp)) => (label.txt, None, argPat(label, mtyp))
  ),
  (
    "unlabeled",
    {|Pattern|},
    (~loc, [@node "Pattern"]pattern) => ("", None, pattern)
  ),
]];

let rec listToConstruct = (~loc, list, maybeRest, construct, tuple, itemLoc) =>
  switch list {
  | [] =>
    switch maybeRest {
    | None => construct(
      ~loc,
      Location.mkloc(Lident("[]"), loc), None)
    | Some(x) => x
    }
  | [one, ...rest] =>
    construct(
      ~loc=itemLoc(one),
      Location.mkloc(Lident("::"), Location.none),
      Some(tuple(
        /* ~loc=itemLoc(one), */
        [one, listToConstruct(~loc=itemLoc(one), rest, maybeRest, construct, tuple, itemLoc)]))
    )
  };

[@name "PatternObjectItem"]
[%%rule [
  (
    "normal",
    {|attribute Pattern|},
    (~loc, [@node "attribute"]attr, [@node "Pattern"]pattern) => (attr, pattern)
  ),
  (
    "punned",
    {|attribute|},
    (~loc, [@node "attribute"]attr) => (attr, H.Pat.var(Location.mkloc(Longident.last(attr.txt), attr.loc)))
  ),
]];

[@name "argLabelWithConstraint"]
[%%rule ("argLabel (':' CoreType)?", (~loc, [@node "argLabel"]ident, [@node_opt "CoreType"]typ) => (ident, typ))]

[@name "argLabel"]
[%%rule ("'~' lowerIdent", ([@text "lowerIdent"](text, loc)) => Location.mkloc(text, loc))];

[@passThrough]
[@name "Parened"]
[%%rule {|"("& Expression & ")"|}];

[@name "attribute"][%%rule ({|':'&$ longIdent|}, ([@node "longIdent"]ident) => ident)];
[@name "shortAttribute"][%%rule {|':'&$ lowerIdent|}];

/** A potentially-namespaced lower-case identifier */
[@name "longIdent"][%%rule (
  {|(longCap_ $&"."&$)? lowerIdent|},
  (~loc, [@node_opt "longCap_"]base, [@text "lowerIdent"](text, _)) => {
    switch base {
    | None => Location.mkloc(Lident(text), loc)
    | Some((base, _loc)) => Location.mkloc(Ldot(base, text), loc)
  }
  }
)];

/** A potentially-namespaced capital identifier */
[@name "longCap"][%%rule (
  {|longCap_ ~"."|},
  ([@node "longCap_"](l, loc)) => Location.mkloc(l, loc)
)];

[@name "longCap_"]
[%%rule [
  (
    "dot",
    {|longCap_ $&"."&$ capIdent|},
    (~loc, [@node "longCap_"](base, _), [@text "capIdent"](text, _)) => (Ldot(base, text), loc)
  ),
  (
    "lident",
    {|capIdent|},
    ([@text "capIdent"](text, loc)) => (Lident(text), loc)
  ),
]];

[@leaf]
[@name "decoratorName"]
[%%rule "decoratorChar+"];

[@name "decoratorChar"]
[%%rule [
  "identchar",
  {|"."|},
  {|"+"|},
  {|"~"|}
]];

let stripQuotes = (str) => String.sub(str, 1, String.length(str) - 2);
let processString = (str) => str |> stripQuotes |> Scanf.unescaped;

[@name "constant"]
[%%rule [
  ("float", {|[val]float|}, ([@text "float"](t, _)) => Const_float(t)),
  ("int", {|[val]int64|}, ([@text "int64"](t, _)) => Const_int(int_of_string(t))),
  ("string", {|ConstString|}, ([@node "ConstString"]t) => t),
  ("longString", {|longString|}, ([@text "longString"](t, _)) => Const_string(String.sub(t, 2, String.length(t) - 4), Some(""))),
  ("char", {|[val]char|}, ([@text "char"](t, _)) => Const_char(t.[0]) /* TODO fixx */
  ),
]];

[@name "polyIdent"][%%rule ("'`' capIdent", ([@text "capIdent"](name, loc)) => Location.mkloc(name, loc))];

/** A simple identifier starting with a capital letter */
[@leaf] [@name "capIdent"][%%rule {|~(reserved ~identchar) 'A..Z' identchar*|}];
/** A simple identifier starting with a lower-case letter */
[@leaf] [@name "lowerIdent"][%%rule {|~(reserved ~identchar) 'a..z' identchar*|}];

[@name "identchar"]
[%%rule [
  "alpha",
  "digit",
  {|"_"|},
]];

[@name "ConstString"][%%rule ("string", ([@text "string"](t, loc)) => Const_string(processString(t), None))];

/** An int constant */
[@leaf] [@name "int64"][%%rule {|digit+ ~identchar|}];
/** A float constant */
[@leaf] [@name "float"][%%rule {|digit+ '.' digit+|}];
/** A string constant */
[@leaf] [@name "string"][%%rule {|"\"" strchar* "\""|}];

[@name "strchar"]
[%%rule [
  {|"\\" any|},
  {|~"\"" ~"\n" ~"\\" any|}
]];

/** Note: This doesn't yet support arbitrary heredoc delimiters. Just {| and |} */
[@leaf] [@name "longString"][%%rule {a|"{|" (~"|}" any)* "|}"|a}];

/** A char constant */
[@leaf] [@name "char"][%%rule {|"'" charchar "'"|}];

[@name "charchar"]
[%%rule [
  {|"\\" any|},
  {|~"'" ~"\n" ~"\\" any|}
]];

[@name "reserved"]
[%%rule [
  {|"fun"|},
  {|"let"|},
  {|"and"|},
  {|"as"|},
  {|"type"|},
  {|"switch"|},
  {|"exception"|},
  {|"external"|},
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
[%%rule [
  {|'a..z'|},
  {|'A..Z'|},
]];

[@name "digit"][%%rule {|'0..9'|}];

/** An operator */
[@leaf] [@name "operator"][%%rule {|~reservedOps opChar+ ~identchar|}];

[@name "reservedOps"]
[%%rule [
  {|"=>"|},
  {|"->"|},
  {|"->>"|},
  {|"..."|},
]];

[@name "opChar"]
[%%rule [
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
  {|"|" ~"]"|},
  {|"~"|},
]];






