
/* ResultUtils */
open Parsetree;
open PackTypes.Result;
open Longident;
open Location;
open Lexing;
let module H = Ast_helper;

let loc = !H.default_loc;
let str = H.Str.eval (H.Exp.array []);

let parseStructure structure => {
  print_endline "parse";
  switch structure.typ {
    | Nonlexical "Structure_value" _ => {
      print_endline "yesh";
      str
    }
    | _ => str
  }
};

let convert result => {
  (List.map parseStructure (ResultUtils.getChildren result.children "structures"));
};
