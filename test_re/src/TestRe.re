/** TODO maybe organize by file? */

module Infix = {
  let (|?>) = (a, b) => switch a { | None => None | Some(x) => b(x) };
  let (|?>>) = (a, b) => switch a { | None => None | Some(x) => Some(b(x)) };
  let (|?) = (a, b) => switch a { | None => b | Some(a) => a };
  let (|??) = (a, b) => switch a { | None => b | Some(a) => Some(a) };
  let (|!) = (a, b) => switch a { | None => failwith(b) | Some(a) => a };
};
open Infix;

type pos = (int, int);

type test('a) = {
  name: string,
  location: (string, pos),
  compare: ('a, 'a) => bool,
  print: option((Format.formatter, 'a) => unit), /* hmmmmm what's up with this */
  diff: option(('a, 'a) => string),
  bools: list((option(string), option(pos), unit => bool)),
  customs: list((option(string), option(pos), unit => option(string))),
  fixtures: list((option(string), option(pos), (unit => 'a, 'a))),
  skip: bool,
  todo: option(string),
};

type abstractTest =
  | Test(test('a)) : abstractTest;

let tests = ref([]);

let add = (
  ~name,
  ~location,
  ~skip=false,
  ~todo=?,
  ~print=?,
  ~diff=?,
  ~compare=(==),
  ~fixtures=[],
  ~named=[],
  ~customs=[],
  ~bools=[],
  ()
) => {
  tests := [Test({
    name,
    location,
    skip,
    todo,
    print,
    compare,
    diff,
    bools,
    customs,
    fixtures
  }), ...tests^]
};

type itemResult =
  | Passed
  | Failed(string)
  | Errored(string, string);

type result =
  | Skipped(abstractTest)
  | Ran(abstractTest, list((option(string), option(pos), itemResult)));

let errored = exn => Errored(Printexc.to_string(exn), Printexc.get_backtrace());

let guard = fn => try { fn () } { | error => errored(error) };

let mapThird = (fn, items) => List.map(((a, b, c)) => (a, b, fn(c)), items);

let showLoc = ((name, (row, col))) => {
  Printf.sprintf("%s:%d:%d", name, row, col)
};

let showTest = ({location, name}) => "'" ++ name ++ "' at " ++ showLoc(location);

/* TODO maybe have a "grep" feature, for only running specific ones  */
/* Probably have a "quiet" mode too. */
let report = () => {
  let results = tests^ |> List.map((Test(test) as t) => {
    if (test.skip || test.todo != None) {
      Skipped(t)
    } else {
      let items =
        mapThird(fn => guard(() => fn() ? Passed : Failed("@test.bool was false")), test.bools)
        @
        mapThird(fn => guard(() => switch (fn()) { | None => Passed | Some(msg) => Failed(msg) }), test.customs)
        @
        mapThird(((fn, expected)) => guard(() => {
          let got = fn();
          if (test.compare(got, expected)) {
            Passed
          } else {
            Failed(switch (test.diff) {
            | None => switch (test.print)  {
              | None => "(no printer provided. add one with @test.print)"
              | Some(printer) => {
                Format.asprintf("\n - got     : %a\n - expected: %a", printer, got, printer, expected)
              }
              }
            | Some(diff) => Format.sprintf("output did not equal expected: %s", diff(got, expected))
            })
          }
        }), test.fixtures);
      Ran(t, items)
    }
  });

  /* TODO have more output options */
  let (total, failed) = results |> List.fold_left(((total, failed), result) => {
    switch result {
    | Skipped(Test(t)) => {print_endline("Skipped " ++ showTest(t)); (total, failed)}
    | Ran(Test(t), items) => {
      /* print_endline("Ran " ++ showTest(t)); */
      items |> List.iteri((i, (name, pos, result)) => switch result {
      | Passed => "."
      | Failed(message) => "\nfailed " ++ showTest(t) ++ " " ++ (name |? string_of_int(i)) ++ ": " ++ message ++ "\n"
      | Errored(message, trace) => "{Error!} " ++ showTest(t) ++ " " ++ (name |? string_of_int(i)) ++ ": " ++ message ++ "\n" ++ trace
      } |> Printf.printf("%s"));
      (total + List.length(items), List.fold_left((x, (_, _, res)) => x + (res == Passed ? 0 : 1), 0, items))
    }
    }
  }, (0, 0));
  if (failed == 0) {
    Printf.printf("\n\n✅  All clear! Ran %d tests\n", total);
  } else {
    Printf.printf("\n\n🛑  Ran %d tests, with %d failures\n", total, failed);
  }
};
