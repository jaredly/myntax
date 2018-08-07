/* open Migrate_parsetree.Ast_406; */

let int_exp = (num) => Ast_helper.Exp.constant(Const_int(num));

let str_exp = (str) => Ast_helper.Exp.constant(Const_string(str, None));

let get_pos = (expr) => {
  open Parsetree;
  let loc = Location.none;
  let pos = expr.Parsetree.pexp_loc.Location.loc_start;
  [%expr
    (
      [%e str_exp(pos.Lexing.pos_fname)],
      [%e int_exp(pos.Lexing.pos_lnum)],
      [%e int_exp(pos.Lexing.pos_cnum - pos.Lexing.pos_bol)]
    )
  ]
};

let process_check = (i, (cname, body)) => {
  open Parsetree;
  let loc = Location.none;
  let cname = Utils.optor(cname, "custom-" ++ string_of_int(i));
  let pos = get_pos(body);
  [%expr ([%e str_exp(cname)], () => [%e body])]
};

let ident = (name) => Ast_helper.Exp.ident(Location.mknoloc(Longident.Lident(name)));

let make_fncall = (name, count) => {
  open Parsetree;
  open Ast_helper;
  /* let loc = Location.none; */
  let rec loop = (n) =>
    if (n < 1) {
      []
    } else {
      [("", ident("arg" ++ string_of_int(count - n + 1))), ...loop(n - 1)]
    };
  Exp.apply(ident(name), loop(count))
};

let make_arg_pattern = (count) => {
  open Parsetree;
  /* let loc = Location.none; */
  switch count {
  | 1 => [%pat ? arg1]
  | _ =>
    let rec loop = (n) =>
      if (n < 1) {
        []
      } else {
        [
          Ast_helper.Pat.var(Location.mknoloc("arg" ++ string_of_int(count - n + 1))),
          ...loop(n - 1)
        ]
      };
    Ast_helper.Pat.tuple(loop(count))
  }
};

/* let test_diff = (test, pos) => {
  /* let loc = Location.none; */
  Test.(
    Parsetree.(
      switch test.diff {
      | Some(expr) => [%expr
          {
            let message = [%e expr];
            let (fname, line, col) = [%e pos];
            (
              (expected, result) =>
                "    custom message: "
                ++ message(expected, result)
                ++ "\n"
                ++ "    at:             "
                ++ fname
                ++ " "
                ++ string_of_int(line)
                ++ ","
                ++ string_of_int(col)
            )
          }
        ]
      | None =>
        switch test.show {
        | Some(expr) => [%expr
            {
              let show = [%e expr];
              let (fname, line, col) = [%e pos];
              (
                (expected, result) =>
                  "    expected: "
                  ++ show(expected)
                  ++ "\n"
                  ++ "    actual:   "
                  ++ show(result)
                  ++ "\n"
                  ++ "    at:       "
                  ++ fname
                  ++ " "
                  ++ string_of_int(line)
                  ++ ","
                  ++ string_of_int(col)
              )
            }
          ]
        | None => [%expr
            {
              let (fname, line, col) = [%e pos];
              (
                (_, _) =>
                  "    unexpected output: (add @@test.show to display)\n"
                  ++ "    at:                "
                  ++ fname
                  ++ " "
                  ++ string_of_int(line)
                  ++ ","
                  ++ string_of_int(col)
              )
            }
          ] /* TODO if bucklescript, to js.log here */
        }
      }
    )
  )
}; */

/* TODO allow multiple fixtures definitions? And just chain them... */
/* let process_fixtures = (fixtures, named, name, test_name, args, test) => {
  open Test;
  open Parsetree;
  let loc = Location.none;
  let call =
    switch test.call {
    | Some(expr) => expr
    | None =>
      switch args {
      | Some(args) => [%expr (([%p make_arg_pattern(args)]) => [%e make_fncall(name, args)])]
      | None => Utils.fail("Must use @@test.call with functions that have labels")
      }
    };
  let fixture_args = named ? [%pat? (input, expected, name)] : [%pat? (input, expected)];
  let compare =
    switch test.compare {
    | Some(expr) => expr
    | None => [%expr ((expected, result) => expected == result)]
    };
  let pos = get_pos(fixtures);
  let diff = test_diff(test, pos);
  let item_name =
    switch test.item_name {
    | Some(expr) => [%expr ((i, input, output) => [%e expr](input, output))]
    | None => [%expr ((i, _, _) => "fixture " ++ string_of_int(i))]
    };
  let fixture_name = named ? [%expr name] : [%expr item_name(i, input, expected)];
  [%expr
    () => {
      let item_name = [%e item_name];
      let compare = [%e compare];
      let diff = [%e diff];
      let call = [%e call];
      (
        [%e str_exp(test_name)],
        List.mapi(
          (i, [%p fixture_args]) => {
            let result = call(input);
            if (compare(expected, result)) {
              ([%e fixture_name], None)
            } else {
              ([%e fixture_name], Some(diff(expected, result)))
            }
          },
          [%e fixtures]
        )
      )
    }
  ]
}; */

let rec make_list = (checks) => {
  open Parsetree;
  /* let loc = Location.none; */
  switch checks {
  | [] => [%expr []]
  | [item, ...rest] => [%expr [[%e item], ...[%e make_list(rest)]]]
  }
};

let getExpected = ({Parsetree.pexp_desc}) => Parsetree.(switch pexp_desc {
| Pexp_construct({txt: Longident.Lident("::")}, Some({pexp_desc: Pexp_tuple([
  {pexp_desc: Pexp_tuple([
    _,
    expected
  ])},
  ..._
])})) => Some(expected)
| _ => None
});

/* TODO infer a bunch more things */

let intPrinter = [%expr (fmt, num) => Format.fprintf(fmt, "%d", num)];
let strPrinter = [%expr (fmt, num) => Format.fprintf(fmt, "%S", num)];

let inferOne = (expr) => Parsetree.(switch (getExpected(expr)) {
| None => None
| Some({pexp_desc: Pexp_constant(Const_int(_))}) => Some(intPrinter)
| Some({pexp_desc: Pexp_constant(Const_string(_))}) => Some(strPrinter)
| _ => None
});

let inferPrinter = (fixtures, named_fixtures) => {
  let rec loop = (f, nf) => switch (f, nf) {
  | ([one, ...rest], two) => switch (inferOne(one)) {
    | None => loop(rest, two)
    | Some(x) => Some(x)
    }
  | ([], [two, ...rest]) =>  switch (inferOne(two)) {
    | None => loop([], rest)
    | Some(x) => Some(x)
    }
  | ([], []) => None
  };
  loop(fixtures, named_fixtures)
};

let test = (name, location, test_name, args, test) => {
  let loc = Location.none;
  open TestType;
  open Parsetree;
  /* let checks = List.mapi(process_check, test.checks);
  let checks = checks @ List.map(expr => process_fixtures(expr, false, name, test_name, args, test), test.fixtures);
  let checks = checks @ List.map(expr => process_fixtures(expr, true, name, test_name, args, test), test.named_fixtures); */
  let (fname, (lnum, cnum)) = location;

  let (argpat, call) = switch test.call {
  | Some(call) => ([%pat? input], [%expr [%e call](input)])
  | None => switch args {
    | Some(args) => (make_arg_pattern(args), make_fncall(name, args))
    | None => failwith("Must have a @test.call when there are named arguments")
    }
  };

  let printer = switch test.print {
  | None => inferPrinter(test.fixtures, test.named_fixtures)
  | Some(p) => Some(p)
  };

  let fixtures = test.fixtures != []
    ? [%expr List.map(
        (([%p argpat], output)) => (None, None, (() => [%e call], output)),
        List.concat([%e make_list(test.fixtures)])
      )]
    : [%expr []];
  let fixtures = test.named_fixtures != []
    ? [%expr [%e fixtures] @ (List.map(
        (([%p argpat], output, name)) => (Some(name), None, (() => [%e call], output)),
        List.concat([%e make_list(test.fixtures)])
      ))] : fixtures;

  let empty = test.fixtures == [] && test.named_fixtures == [];

  if (empty) {
    None
  } else {
    Some([%stri
      TestRe.add(
        ~name=[%e str_exp(name)],
        ~location=([%e str_exp(fname)], ([%e int_exp(lnum)], [%e int_exp(cnum)])),
        ~skip=false,
        ~todo=?None,
        ~print=?[%e switch printer {
        | None => [%expr None]
        | Some(print) => [%expr Some([%e print])]
        }],
        ~diff=?None,
        ~compare=(==),
        ~fixtures=[%e fixtures],
        ()
      )
    ])
  }
};
