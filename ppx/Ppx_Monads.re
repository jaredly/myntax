

/***
 * https://ocsigen.org/lwt/dev/api/Ppx_lwt
 * https://github.com/zepalmer/ocaml-monadic
 */
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

open Parsetree;

/* open Belt.Result; */

let strExpr = str => switch str.pstr_desc {
  | Pstr_eval(expr, _) => expr
  | _ => fail(str.pstr_loc, "Expected an expression")
};

let tupleOrSingle = str => switch str.pstr_desc {
  | Pstr_eval({pexp_desc: Pexp_tuple(items)}, _) => `Tuple(items)
  | Pstr_eval(expr, _) => `Single(expr)
  | _ => fail(str.pstr_loc, "Expected a tuple or an expression")
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

let startsWith = (s, prefix) => {
  if (prefix == "") {
    true
  } else {
    let p = String.length(prefix);
    p <= String.length(s) && String.sub(s, 0, p) == prefix
  }
};

let converterExpr = (fn) => {
  let rec loop = (expr, args) => {
    switch expr.pexp_desc {
      | Pexp_fun(label, default, pattern, res) => {
        let arg = if (label == "loc") {
          [%expr _loc]
        } else {
          switch (pattern.ppat_attributes) {
            | [({txt: "text"}, PStr([str]))] => {
              let name = strString(str);
              [%expr
              switch (ResultUtils.getLeafByType(children, [%e strExp(name)])) {
                | None => failwith("Expected a " ++ [%e strExp(name)])
                | Some((contents, loc)) => (contents, loc)
              }
              ]
            }
            | [({txt: "node_opt"}, PStr([str]))] => {
              let name = strString(str);
                [%expr
                switch (ResultUtils.getNodeByType(children, [%e strExp(name)])) {
                  | None => None
                  | Some(node) => Some([%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))](node))
                }]
            }
            | [({txt}, PStr([str]))] when startsWith(txt, "node_opt.") => {
              let name = strString(str);
              let label = String.sub(txt, 9, String.length(txt) - 9);
                [%expr
                switch (ResultUtils.getNodeByLabel(children, [%e strExp(label)])) {
                  | None => None
                  | Some(((_, sub), children, loc)) => Some([%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))]((sub, children, loc)))
                }
              ]
            }
            | [({txt: "node"}, PStr([str]))] => {
              let name = strString(str);
                [%expr
                switch (ResultUtils.getNodeByType(children, [%e strExp(name)])) {
                  | None => failwith("Expected a " ++ [%e strExp(name)])
                  | Some(node) => [%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))](node)
                }
              ]
            }
            | [({txt}, PStr([str]))] when startsWith(txt, "node.") => {
              let name = strString(str);
              let label = String.sub(txt, 5, String.length(txt) - 5);
                [%expr
                switch (ResultUtils.getNodeByLabel(children, [%e strExp(label)])) {
                  | None => failwith("Expected a " ++ [%e strExp(name)])
                  | Some(((_, sub), children, loc)) => [%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))]((sub, children, loc))
                }
              ]
            }
            | [({txt}, PStr([str]))] when startsWith(txt, "nodes.") => {
              let name = strString(str);
              let label = String.sub(txt, 6, String.length(txt) - 6);
                [%expr
                  ResultUtils.getNodesByLabel(children, [%e strExp(label)], [%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))])
              ]
            }
            | [({txt: "nodes"}, PStr([str]))] => {
              let name = strString(str);
                [%expr
                ResultUtils.getNodesByType(children, [%e strExp(name)],
                [%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))])
              ]
            }
            | [({txt: "texts"}, PStr([str]))] => {
              let name = strString(str);
                [%expr
                ResultUtils.getLeafsByType(children, [%e strExp(name)])
              ]
            }
            | _ => switch (pattern.ppat_desc) {
              | Ppat_construct({txt: Lident("()")}, _)
              | Ppat_any => [%expr ()]
              | _ => fail(pattern.ppat_loc, "Arguments must be annotated to indicate how to fulfill the values")
            }
          };
        };
        let (expr, _) = loop(res, [("", arg), ...args]);
        (Ast_helper.Exp.let_(Nonrecursive, [Ast_helper.Vb.mk(pattern, arg)], expr), [])
      }
      | _ => (expr, args)
    }
  };
  let (expr, args) = loop(fn, []);
  /* let args = args |> List.rev;
  Ast_helper.Exp.apply(fn, args) */
  expr
};

let maybeConvertChoice = choice => switch (choice.pexp_desc) {
  | Pexp_constant(Const_string(name, _)) => {
    [%expr Grammar.choice([%e choice])]
  }
  | _ => choice
};

let mapper = _argv =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: (mapper, items) => {
      let (top, rules, converters, found) = List.fold_left(((top, rules, converters, found), item) => {
        switch item.pstr_desc {
          | Parsetree.Pstr_extension(({txt: ("rule" | "rules" | "passThroughRule") as txt}, contents), attributes) => {
            let name = switch (attrString(attributes, "name")) {
              | None => fail(item.pstr_loc, "No name for rule")
              | Some(name) => name
            };
            let passThrough = switch (attrBool(attributes, "passThrough")) {
              | None => false
              | Some(n) => n
            };
            let leaf = switch (attrBool(attributes, "leaf")) {
              | None => false
              | Some(n) => n
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
                passThrough: [%e Ast_helper.Exp.construct(
                  Location.mknoloc(Longident.Lident(passThrough ? "true" : "false")),
                  None
                )],
                ignoreNewlines: [%e ignoreNewlines],
                leaf: [%e Ast_helper.Exp.construct(
                  Location.mknoloc(Longident.Lident(leaf ? "true" : "false")),
                  None
                )],
                choices: [%e choices]
              }), ...[%e rules]]];

            if (txt == "passThroughRule") {
              let choice = switch contents {
                | [one] => strExpr(one)
                | _ => fail(item.pstr_loc, "Must contain a single rule")
              };
              (top, newRules([%expr [("", "", [%e maybeConvertChoice(choice)])]]), converters, true)
            } else if (txt == "rule") {
              let choice = switch contents {
                | [one] => tupleOrSingle(one)
                | _ => fail(item.pstr_loc, "Must contain a single tuple")
              };
              switch choice {
                | `Single(choice) =>
                  (top, newRules([%expr [("", "", [%e maybeConvertChoice(choice)])]]), converters, true)
                | `Tuple([choice, fn]) =>
                  let fnCall = converterExpr(fn);
                  let converter: (string, Parsetree.expression) = (
                    name,
                    [%expr ((sub, children, _loc)) => [%e fnCall]]
                  );
                  (top, newRules([%expr [("", "", [%e maybeConvertChoice(choice)])]]), [converter, ...converters], true)
                | `Tuple(_) => fail(item.pstr_loc, "Expected a tuple of two items")
              }
            } else if (txt == "rules") {

              let body = switch contents {
                | [one] => listItems(one)
                | _ => fail(item.pstr_loc, "Must contain a single list")
              };
              let (rules, cases) = body |. Belt.List.reduceReverse(([%expr []], []), ((rules, cases), expr) => {
                switch (expr.pexp_desc) {
                  | Pexp_tuple([
                    {pexp_desc: Pexp_constant(Const_string(name, _))},
                    rule,
                    fn
                  ]) => {
                    let fnCall = converterExpr(fn);
                    ([%expr [([%e strExp(name)], "", [%e maybeConvertChoice(rule)]), ...[%e rules]]], [Ast_helper.Exp.case(
                      Ast_helper.Pat.constant(Const_string(name, None)),
                      fnCall
                    ), ...cases])
                  }
                  | Pexp_constant(Const_string(contents, _)) => {
                    ([%expr [("", "", [%e maybeConvertChoice(expr)]), ...[%e rules]]], cases)
                  }
                  | _ => fail(expr.pexp_loc, "Invalid rule item")
                }
              });
              let cases = cases @ [
                Ast_helper.Exp.case(
                  Ast_helper.Pat.any(),
                  Ast_helper.Exp.assert_(Ast_helper.Exp.construct(Location.mknoloc(Longident.Lident("false")), None))
                  
                )
              ];

              let sw = Ast_helper.Exp.match([%expr sub], cases);
              let converter  = (
                name,
                [%expr ((sub, children, _loc)) => [%e sw]]
              );
              (top, newRules(rules), [converter, ...converters], true)

              /* switch body {
                | [choice, fn] =>
                  let fnCall = converterExpr(fn);
                  let converter: (string, Parsetree.expression) = (
                    name,
                    [%expr ((sub, children, _loc)) => [%e fnCall]]
                  );
                  (top, newRules([%expr [("", "", [%e choice])]]), [converter, ...converters], true)
                | _ => fail(item.pstr_loc, "Expected a tuple of two items")
              }; */

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