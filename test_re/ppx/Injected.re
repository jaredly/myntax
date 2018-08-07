/* open Migrate_parsetree.Ast_406; */

let rec blank = (num) =>
  if (num == 0) {
    ""
  } else {
    " " ++ blank(num - 1)
  };

let newline = Str.regexp("\n");

let indent = (num, text) => {
  let white = blank(num);
  white ++ Str.global_replace(newline, white ++ "\n", text)
};

let tap_reporter = {
  let loc = Location.none;
  Parsetree.(
    [%stri
      let _ = {
        print_endline("TAP version 13");
        let si = string_of_int;
        let (total, errs) =
          List.fold_right(
            ((name, fns), (total, errs)) => {
              print_endline("# " ++ name);
              List.fold_left(
                ((total, errs), fn) => {
                  let (name, results) = fn();
                  List.fold_left(
                    ((total, errs), (subname, failure)) => {
                      let description = string_of_int(total + 1) ++ " " ++ subname;
                      /*print_endline ("test " ^ name ^ " : " ^ subname);*/
                      switch failure {
                      | None =>
                        print_endline("ok " ++ description);
                        (total + 1, errs)
                      | Some(text) =>
                        print_endline("not ok " ++ description);
                        print_endline("  ---");
                        print_endline(text);
                        print_endline("  ...");
                        (total + 1, errs + 1)
                      }
                    },
                    (total, errs),
                    results
                  )
                },
                (total, errs),
                fns
              )
            },
            tests^,
            (0, 0)
          );
        print_endline("1.." ++ si(total));
        print_endline("# tests " ++ si(total));
        print_endline("# pass " ++ si(total - errs));
        print_endline("# fail " ++ si(errs))
      }
    ]
  )
};
/*let make_final_test_block tests => {
    let loc = Location.none;
    open Parsetree;
    [%stri let _ = {
      let (total, errs) = List.fold_left
      (fun (total, errs) (name, fns) => {
        print_endline ("section: " ^ name);
        List.fold_left
        (fun (total, errs) fn => {
          let (name, results) = fn ();
          List.fold_left
          (fun (total, errs) (subname, failure) => {
            print_endline ("test " ^ name ^ " : " ^ subname);
            switch failure {
            | None => (total + 1, errs)
            | Some text => {
              /*print_endline name;*/
              /*print_endline subname;*/
              print_endline text;
              (total + 1, errs + 1)
            }
            }
          })
          (total, errs)
          results
        })
        (total, errs)
        fns
      })
      (0, 0)
      !tests;
      print_endline ("Done! " ^ (string_of_int errs) ^ " failed out of " ^ (string_of_int total))
    }]
  };*/
