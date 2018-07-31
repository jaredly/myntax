
open Asttypes;
open Parsetree;
module H = Ast_helper;

module DSL = PackTypes.DSL;

/* let rec convert_Start = ((sub, children, loc)) => {
  let structure = ResultUtils.getNodesByType(children, "Structure", convert_Structure);
  structure
} and convert_Structure = ((sub, children, loc)) => {
  switch sub {
    | "let" =>
      let pair = ResultUtils.getNodeByType(children, "LetPair") |?>> convert_LetPair |! "No letpair found";
      H.Str.value(~loc, Nonrecursive, [pair]);
    | "module" =>
      let (name, nameLoc) = ResultUtils.getLeafByType(children, "capIdent");
      let expr = ResultUtils.getNodeByTypes("ModuleExpr") |?>> convert_ModuleExpr |! "No module expr";
      H.Str.module_(~loc, H.Mb.mk(
        Location.mkloc(name, nameLoc),
        expr
      ))
    | _ => assert(false)
  }
}; */

/* [@lineComment ";"];
[@blockComment ("(**", "*)")];

[@name "Start"]
[%%rule (
  [DSL.n("ModuleBody")],
  ([@nodes "Structure"]structures) => structures
)];

[@name "ModuleBody"]
[%%passThroughRule (
  DSL.([star(n("Structure"))]),
)];

let lp = DSL.(hugRight(t("(")));
let rp = DSL.(hugLeft(t(")")));

[@name "Structure"]
[%%rules [
  ("let", DSL.([lp, t("def"), n("LetPair"), rp]),
    (~loc, [@node "LetPair"]pair) => H.Str.value(~loc, Nonrecursive, [pair])
  ),
  ( "let_rec", DSL.([lp, t("def-rec"), plus(n("LetPair")), rp]),
    (~loc, [@nodes "LetPair"]pairs) => H.Str.value(~loc, Recursive, pairs)
  ),
  ( "type", DSL.([lp, t("type"), n("TypeBody"), rp]),
    (~loc, [@nodes "TypePair"]pairs) => H.Str.type_(pairs),
  ),
  ( "module", DSL.([lp, t("module"), n("capIdent"), n("ModuleExpr"), rp]),
    (~loc, [@text "capIdent"](name, nameLoc), [@node "ModuleExpr"]expr) => H.Str.module_(~loc, H.Mb.mk(
      Location.mkloc(name, nameLoc),
      expr
    )),
  ),
  ( "open", DSL.([lp, t("open"), n("longCap"), rp]),
    (~loc, [@node "longCap"]lident) => H.Str.open_(~loc, H.Opn.mk(lident))
  ),
  ( "eval", DSL.([n("Expression")]),
    (~loc, [@node "Expression"]expr) => H.Str.eval(~loc, expr)
  )
]]; */
