

/***
 * https://ocsigen.org/lwt/dev/api/Ppx_lwt
 * https://github.com/zepalmer/ocaml-monadic
 */
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

/* let rec process_bindings = (bindings) =>
  Parsetree.(
    switch bindings {
    | [] => assert false
    | [binding] => (binding.pvb_pat, binding.pvb_expr)
    | [binding, ...rest] =>
      let (pattern, expr) = process_bindings(rest);
      (
        Ast_helper.Pat.tuple([binding.pvb_pat, pattern]),
        [%expr Let_syntax.join2([%e binding.pvb_expr], [%e expr])]
      )
    }
  );

let process_let = (contents, loc) => {
  open Parsetree;
  let bindings =
    switch contents {
    | PStr([{pstr_desc: Pstr_value(Nonrecursive, bindings), pstr_loc}]) => bindings
    | _ => fail(loc, "extension must contain a nonrecursive let binding")
    };
  process_bindings(bindings)
};

let getExpr = (contents, loc) =>
  Parsetree.(
    switch contents {
    | PStr([{pstr_desc: Pstr_eval(expr, _)}]) => expr
    | _ => fail(loc, "@else must contain an expression")
    }
  ); */

let opt_explanation = {|
Optional declaration sugar:
```
let%opt name = value;
otherStuff
```
is transformed into
```
switch (value) {
| None => None
| Some(name) =>
  otherStuff
}
```
This means that `otherStuff` needs to have type `option`.

If you want `otherStuff` to be automatically wrapped in `Some()`,
then use `let%opt_wrap`.
Alternatively, if you are just performing a side effect, and want
the result of the whole thing to be unit, use `let%consume`.
|};

let opt_wrap_explanation = {|
Optional declaration sugar:
```
let%opt_wrap name = value;
otherStuff
```
is transformed into
```
switch (value) {
| None => None
| Some(name) => Some({
    otherStuff
  })
}
```
The `wrap` suffix means that the `otherStuff` will be automatically
wrapped in a `Some`.

If you don't want this wrapping, then use `let%opt`.
Alternatively, if you are just performing a side effect, and want
the result of the whole thing to be unit, use `let%consume`.
|};

let opt_consume_explanation = {|
Optional declaration sugar:
```
let%consume name = value;
otherStuff
```
is transformed into
```
switch (value) {
| None => ()
| Some(name) =>
  otherStuff
}
```
This is intented for performing side-effects only -- `otherStuff`
must end up as type `unit`.
|};

open Parsetree;

/* open Belt.Result; */

let strExpr = str => switch str.pstr_desc {
  | Pstr_eval(expr, _) => expr
  | _ => fail(str.pstr_loc, "Expected an expression")
};

let tupleItems = str => switch str.pstr_desc {
  | Pstr_eval({pexp_desc: Pexp_tuple(items)}, _) => items
  | _ => fail(str.pstr_loc, "Expected a tuple")
};

let strString = str => switch str.pstr_desc {
  | Pstr_eval({pexp_desc: Pexp_constant(Const_string(text, _))}, _) => text
  | _ => fail(str.pstr_loc, "Expected a string")
};

let rec unrollList = exp => switch exp.pexp_desc {
  | Pexp_construct({txt: Lident("[]")}, None) => []
  | Pexp_construct({txt: Lident("::")}, Some({pexp_desc: Pexp_tuple([head, tail])})) => [head, ...unrollList(tail)]
  | _ => []
};

let listItems = str => switch str.pstr_desc {
  | Pstr_eval({pexp_desc: Pexp_construct({txt: Lident("::")}, Some({pexp_desc: Pexp_tuple([head, tail])}))}, _) => {
    [head, ...unrollList(tail)]
  }
  | _ => fail(str.pstr_loc, "Expected a list")
};

let attrString = (attrs, name) => {
  let rec loop = (items) => switch items {
    | [] => None
    | [({Location.txt, loc}, contents), ..._] when txt == name => switch contents {
      | PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_constant(Const_string(text, _))}, _)}]) => Some(text)
      | _ => fail(loc, "Attr " ++ name ++ " must be a string")
    }
    | [_, ...rest] => loop(rest)
  };
  loop(attrs)
};

let attrBool = (attrs, name) => {
  let rec loop = (items) => switch items {
    | [] => None
    | [({Location.txt, loc}, contents), ..._] when txt == name => switch contents {
      | PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_construct({txt: Lident("false")}, None)}, _)}]) => Some(false)
      | PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_construct({txt: Lident("true")}, None)}, _)}]) => Some(true)
      | PStr([]) => Some(true)
      | _ => fail(loc, "Attr " ++ name ++ " must be a bool or empty")
    }
    | [_, ...rest] => loop(rest)
  };
  loop(attrs)
};

let strExp = name => Ast_helper.Exp.constant(Const_string(name, None));
let identExp = (~loc=Location.none, lident) => Ast_helper.Exp.ident(~loc, Location.mkloc(lident, loc));

let converterExpr = (fn) => {
  let rec loop = (expr, args) => {
    switch expr.pexp_desc {
      | Pexp_fun(label, default, pattern, res) => {
        if (label == "loc") {
          loop(res, [(label, [%expr _loc]), ...args])
        } else {
          switch (pattern.ppat_attributes) {
            | [({txt: "node"}, PStr([str]))] => {
              let name = strString(str);
              loop(res, [
                ("", [%expr
                switch (ResultUtils.getNodeByType(children, [%e strExp(name)])) {
                  | None => failwith("Expected a " ++ [%e strExp(name)])
                  | Some(node) => [%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))](node)
                }
                ]),
                ...args
              ])
            }
            | [({txt: "nodes"}, PStr([str]))] => {
              let name = strString(str);
              loop(res, [
                ("", [%expr
                ResultUtils.getNodesByType(children, [%e strExp(name)],
                [%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))])
                ]),
                ...args
              ])
            }
            | _ => failwith("Arguments must be annotated to indicate how to fulfill the values")
          }
        }
      }
      | _ => args
    }
  };
  let args = loop(fn, []) |> List.rev;
  Ast_helper.Exp.apply(fn, args)
};

let mapper = _argv =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: (mapper, items) => {
      let (top, rules, converters, found) = List.fold_left(((top, rules, converters, found), item) => {
        switch item.pstr_desc {
          | Parsetree.Pstr_extension(({txt: ("rule" | "rules" | "passThroughRule") as txt}, contents), attributes) => {
            let name = switch (attrString(attributes, "name")) {
              | None => failwith("No name for rule")
              | Some(name) => name
            };
            let ignoreNewlines = switch (attrBool(attributes, "ignoreNewlines")) {
              | None => [%expr Inherit]
              | Some(true) => [%expr Yes]
              | Some(false) => [%expr No]
            };
            let contents = switch contents {
              | PStr(str) => str
              | _ => fail(item.pstr_loc, "Contents must not be a type or pattern")
            };

            let newRules = choices => [%expr [([%e strExp(name)], {
                passThrough: true,
                ignoreNewlines: [%e ignoreNewlines],
                leaf: false,
                choices: [%e choices]
              }), ...[%e rules]]];

            if (txt == "passThroughRule") {
              let choice = switch contents {
                | [one] => strExpr(one)
                | _ => fail(item.pstr_loc, "Must contain a single structure item")
              };
              (top, newRules([%expr [("", "", [%e choice])]]), converters, true)
            } else if (txt == "rule") {
              let body = switch contents {
                | [one] => tupleItems(one)
                | _ => fail(item.pstr_loc, "Must contain a single structure item")
              };
              switch body {
                | [choice, fn] =>
                  let fnCall = converterExpr(fn);
                  let converter: (string, Parsetree.expression) = (
                    name,
                    [%expr ((sub, children, _loc)) => [%e fnCall]]
                  );
                  (top, newRules([%expr [("", "", [%e choice])]]), [converter, ...converters], true)
                | _ => fail(item.pstr_loc, "Expected a tuple of two items")
              };
            } else {
              (top, rules, converters, found)
            }
          }
          | _ => ([mapper.structure_item(mapper, item), ...top], rules, converters, found)
        }

      }, ([], [%expr []], [], false), items);
      let top = List.rev(top);
      if (found) {
        let grammar = [%str let grammar = PackTypes.Parsing.{
          lineComment: Some(";"),
          blockComment: Some(("(**", "*)")),
          rules: [%e rules]
        }];
        let converters = Ast_helper.Str.value(
          Recursive,
          converters |. Belt.List.map(((name, contents)) => {
            Ast_helper.Vb.mk(
              Ast_helper.Pat.var(Location.mknoloc("convert_" ++ name)),
              contents
            )
          })
        );
        top @ grammar @ [converters]
      } else {
        top
      }
      /* Now make a let rec for all the converters... and a Grammar™ for the rules */
    },
    /* x: switch expr.pexp_desc {
      | Pexp_extension(({txt: (
        "opt" | "opt_wrap" | "opt_consume" | "opt_force"
        | "try" | "try_wrap" | "try_consume" | "try_force"
        | "await" | "await_wrap" | "await_consume"
        ) as txt, loc}, PStr([{pstr_desc: Pstr_eval({pexp_desc: Pexp_let(Nonrecursive, bindings, continuation)}, attributes)}]))) => {
        let (front, explanation) = switch (txt) {
          | "opt" => ([%expr Monads.Option.bind], opt_explanation)
          | "opt_wrap" => ([%expr Monads.Option.map], opt_wrap_explanation)
          | "opt_consume" => ([%expr Monads.Option.consume], opt_consume_explanation)
          | "opt_force" => ([%expr Monads.Option.force], "Force an optional. Throws an error if None")
          | "try" => ([%expr Monads.Result.bind], "Sugar for the Result type")
          | "try_wrap" => ([%expr Monads.Result.map], "Sugar for the Result type - auto-wraps in `Ok()`")
          | "try_consume" => ([%expr Monads.Result.consume], "Sugar for the Result type - side-effectful version")
          | "try_force" => ([%expr Monads.Result.force], "Sugar for the Result type - force a result")
          | "await" => ([%expr Monads.Promise.bind], "Sugar for Promises")
          | "await_wrap" => ([%expr Monads.Promise.map], "Sugar for Promises - auto-wraps in `Promise.resolve`")
          | "await_consume" => ([%expr Monads.Promise.consume], "Sugar for Promises - just for side effects. Throws on error")
          | _ => assert(false)
        };
        let (pat, expr) = process_bindings(bindings);
        Ast_helper.Exp.attr(
          [%expr [%e front]([%e mapper.expr(mapper, expr)], ~f=([%p pat]) => [%e mapper.expr(mapper, continuation)])],
          ({txt: "ocaml.explanation", loc}, PStr([
            Ast_helper.Str.eval(Ast_helper.Exp.constant(Const_string(explanation, None)))
          ]))
        )
      }
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      } */
  };

let () = Ast_mapper.run_main(mapper);