
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

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

let ruleBody = str => switch str.pstr_desc {
  | Pstr_eval({pexp_desc: Pexp_tuple(items)}, _) => `Tuple(items)
  | Pstr_eval({pexp_desc: Pexp_construct({txt: Lident("::")}, Some({pexp_desc: Pexp_tuple([head, tail])}))}, _) => {
    `List([head, ...unrollList(tail)])
  }
  | Pstr_eval(expr, _) => `Single(expr)
  | _ => fail(str.pstr_loc, "Expected a tuple or a list of tuples or an expression")
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

let converterExpr = (~name as ruleName, fn) => {
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
                | None => raise(PackTypes.ConversionError(_loc, [%e strExp(ruleName)], [%e strExp(name)]))
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
              [%expr switch (ResultUtils.getNodeByType(children, [%e strExp(name)])) {
                | None => raise(PackTypes.ConversionError(_loc, [%e strExp(ruleName)], [%e strExp(name)]))
                | Some(node) => [%e identExp(~loc=str.pstr_loc, Lident("convert_" ++ name))](node)
              }]
            }
            | [({txt}, PStr([str]))] when startsWith(txt, "node.") => {
              let name = strString(str);
              let label = String.sub(txt, 5, String.length(txt) - 5);
                [%expr
                switch (ResultUtils.getNodeByLabel(children, [%e strExp(label)])) {
                  | None => raise(PackTypes.ConversionError(_loc, [%e strExp(ruleName)], [%e strExp(name)]))
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
        let ploc = switch (pattern.ppat_attributes) {
          | [({loc}, _)] => loc
          | _ => pattern.ppat_loc
        };
        let (expr, _) = loop(res, [("", arg), ...args]);
        let arg = {...arg, pexp_loc: ploc};
        (Ast_helper.Exp.let_(~loc=ploc, Nonrecursive, [Ast_helper.Vb.mk(~loc=ploc, pattern, arg)], expr), [])
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

let (|?) = IInfix.(|?);

let mapper = _argv =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    structure: (mapper, items) => {
      let (top, rules, converters, found) = List.fold_left(((top, rules, converters, found), item) => {
        switch item.pstr_desc {
          | Parsetree.Pstr_extension(({txt: "rule"}, contents), attributes) => {
            let name = switch (attrString(attributes, "name")) {
              | None => fail(item.pstr_loc, "No name for rule")
              | Some(name) => name
            };
            let docs = attrString(attributes, "ocaml.doc");
            let passThrough = attrBool(attributes, "passThrough") |? false;
            let capturesComments = attrBool(attributes, "capturesComments") |? false;
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
              capturesComments: [%e Ast_helper.Exp.construct(
                Location.mknoloc(Longident.Lident(capturesComments ? "true" : "false")),
                None
              )],
              passThrough: [%e Ast_helper.Exp.construct(
                Location.mknoloc(Longident.Lident(passThrough ? "true" : "false")),
                None
              )],
              docs: [%e switch docs { | None => [%expr None] | Some(x) => [%expr Some([%e strExp(x)])]}],
              ignoreNewlines: [%e ignoreNewlines],
              leaf: [%e Ast_helper.Exp.construct(
                Location.mknoloc(Longident.Lident(leaf ? "true" : "false")),
                None
              )],
              choices: [%e choices]
            }), ...[%e rules]]];

            let choice = switch contents {
              | [one] => ruleBody(one)
              | _ => fail(item.pstr_loc, "Must contain a single item")
            };
            let docs = docs |? "";
            switch choice {
            | `Single(choice) =>
              (top, newRules([%expr [("", [%e strExp(docs)], [%e maybeConvertChoice(choice)])]]), converters, true)
            | `Tuple([choice, fn]) =>
              let fnCall = converterExpr(~name, fn);
              let converter: (string, Parsetree.expression) = (
                name,
                [%expr ((sub, children, _loc)) => [%e fnCall]]
              );
              let ruleDocs = attrString(choice.pexp_attributes, "ocaml.doc") |? docs;
              (top, newRules([%expr [("", [%e strExp(ruleDocs)], [%e maybeConvertChoice(choice)])]]), [converter, ...converters], true)
            | `Tuple(_) => fail(item.pstr_loc, "Expected a tuple of two items")
            | `List(body) =>
              let (rules, cases) = body |. Belt.List.reduceReverse(([%expr []], []), ((rules, cases), expr) => {
                switch (expr.pexp_desc) {
                  | Pexp_tuple([
                    {pexp_desc: Pexp_constant(Const_string(subName, _))},
                    rule,
                    fn
                  ]) => {
                    let fnCall = converterExpr(~name=name ++ ":" ++ subName, fn);
                    let ruleDocs = attrString(expr.pexp_attributes, "ocaml.doc") |? "";
                    ([%expr [([%e strExp(subName)], [%e strExp(ruleDocs)], [%e maybeConvertChoice(rule)]), ...[%e rules]]], [Ast_helper.Exp.case(
                      Ast_helper.Pat.constant(Const_string(subName, None)),
                      fnCall
                    ), ...cases])
                  }
                  | Pexp_constant(Const_string(contents, _)) => {
                    let ruleDocs = attrString(expr.pexp_attributes, "ocaml.doc") |? "";
                    ([%expr [("", [%e strExp(ruleDocs)], [%e maybeConvertChoice(expr)]), ...[%e rules]]], cases)
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
        };
        let start = (~filename, text) => {
          Runtime.fname := filename;
          switch (Runtime.parse(grammar, "Start", text)) {
          | Belt.Result.Error((Some(Node(("Start", sub), children, loc)), e)) =>
            Belt.Result.Error((Some(convert_Start((sub, children, loc))), e))
          | Belt.Result.Error((_, e)) => Belt.Result.Error((None, e))
          | Ok(Node(("Start", sub), children, loc)) => {
            Ok(convert_Start((sub, children, loc)));
          }
          | Ok(_) => failwith("Invalid response")
          }
        }
        ];
        let converters = Ast_helper.Str.value(
          Recursive,
          converters |. Belt.List.map(((name, contents)) => {
            Ast_helper.Vb.mk(
              Ast_helper.Pat.var(Location.mknoloc("convert_" ++ name)),
              contents
            )
          })
        );
        top @ [converters] @ grammar
      } else {
        top
      }
    },
  };

let () = Ast_mapper.run_main(mapper);