/* open Migrate_parsetree.Ast_406; */

type t = {
  fixtures: list(Parsetree.expression),
  named_fixtures: list(Parsetree.expression),
  item_name: option(Parsetree.expression),
  skipif: option(Parsetree.expression),
  skip: option(string),
  only: bool,
  todo: option(string),
  call: option(Parsetree.expression),
  diff: option(Parsetree.expression),
  print: option(Parsetree.expression),
  compare: option(Parsetree.expression),
  customs: list((option(string), Parsetree.expression)),
  bools: list(Parsetree.expression),
  location: (string, (int, int)),
  name: option(string)
};

let empty = {
  item_name: None,
  fixtures: [],
  named_fixtures: [],
  call: None,
  diff: None,
  print: None,
  compare: None,
  customs: [],
  bools: [],
  name: None,
  skip: None,
  todo: None,
  skipif: None,
  location: ("", (0, 0)),
  only: false
};

let validate = (test) =>
  switch test {
  | {
      fixtures: [],
      named_fixtures: [],
      diff: None,
      print: None,
      compare: None,
      customs: [],
      name: None,
      call: None
    } =>
    None
  | {fixtures: [], named_fixtures: [], diff: Some(_)}
  | {fixtures: [], named_fixtures: [], print: Some(_)}
  | {fixtures: [], named_fixtures: [], call: Some(_)}
  | {fixtures: [], named_fixtures: [], compare: Some(_)} =>
    Utils.fail("Doesn't make sense to not have fixtures")
  | {diff: Some(_), print: Some(_)} => Utils.fail("Diff and print together doesn't make sense")
  | {fixtures: [], named_fixtures: [], customs: []} =>
    Utils.fail("Partial attributes.. doesn't make sense")
  | _ => Some(test)
  };
