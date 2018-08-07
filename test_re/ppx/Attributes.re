/* open Migrate_parsetree.Ast_406; */

open TestType;

let payload_expr = (payload) =>
  switch payload {
  | Parsetree.PStr([{pstr_desc: Pstr_eval(expr, _)}]) => Some(expr)
  | _ => None
  };

let require_payload_expr = (payload) =>
  switch (payload_expr(payload)) {
  | None => Utils.fail("@@test payload must be an expression")
  | Some(expr) => expr
  };

let require_payload_string = (payload) =>
  switch payload {
  | Parsetree.PStr([
      {pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(value, None))}, [])}
    ]) => value
  | _ => Utils.fail("@@test.name payload must be a string")
  };

/* TODO if the first thing is a string literal, grab that out & use it as a name */
let process_check_expr = (expr) => (None, expr);

let process = (attributes) =>
  List.fold_left(
    (test, (name, payload)) =>
      switch name.Location.txt {
      | "test" =>
        let expr = require_payload_expr(payload);
        {...test, fixtures: [expr, ...test.fixtures]}
      | "test.named" =>
        let expr = require_payload_expr(payload);
        {...test, named_fixtures: [expr, ...test.named_fixtures]}
      | "test.item_name" =>
        let expr = require_payload_expr(payload);
        switch test.item_name {
        | None => {...test, item_name: Some(expr)}
        | _ => Utils.fail("multiple @@test.item_name annotations found")
        }
      | "test.call" =>
        let expr = require_payload_expr(payload);
        switch test.call {
        | None => {...test, call: Some(expr)}
        | _ => Utils.fail("multiple @@test.call annotations found")
        }
      | "test.print" =>
        let expr = require_payload_expr(payload);
        switch test.print {
        | None => {...test, print: Some(expr)}
        | _ => Utils.fail("multiple @@test.print annotations found")
        }
      | "test.diff" =>
        let expr = require_payload_expr(payload);
        switch test.diff {
        | None => {...test, diff: Some(expr)}
        | _ => Utils.fail("multiple @@test.diff annotations found")
        }
      | "test.compare" =>
        let expr = require_payload_expr(payload);
        switch test.compare {
        | None => {...test, compare: Some(expr)}
        | _ => Utils.fail("multiple @@test.compare annotations found")
        }
      | "test.skipif" =>
        let expr = require_payload_expr(payload);
        switch test.skipif {
        | None => {...test, skipif: Some(expr)}
        | _ => Utils.fail("multiple @@test.skipif annotations found")
        }
      | "test.skip" =>
        let name = require_payload_string(payload);
        switch test.skip {
        | None => {...test, skip: Some(name)}
        | _ => Utils.fail("multiple @@test.skip annotations found")
        }
      | "test.todo" =>
        let name = require_payload_string(payload);
        switch test.todo {
        | None => {...test, todo: Some(name)}
        | _ => Utils.fail("multiple @@test.todo annotations found")
        }
      | "test.name" =>
        let name = require_payload_string(payload);
        switch test.name {
        | None => {...test, name: Some(name)}
        | _ => Utils.fail("multiple @@test.name annotations found")
        }
      | "test.bool" =>
        let expr = require_payload_expr(payload);
        {...test, bools: [expr, ...test.bools]}
      | "test.custom" =>
        let expr = require_payload_expr(payload);
        {...test, customs: [process_check_expr(expr), ...test.customs]}
      | _ => test
      },
    TestType.empty,
    attributes
  );
