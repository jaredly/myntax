/*
 /* Assuming rewriter is written against OCaml 4.04 parsetree */
 let migration =
   Migrate_parsetree.Versions.migrate Migrate_parsetree.Versions.ocaml_404 Migrate_parsetree.Versions.ocaml_current;

 let () =
   /* Refer to unshadowed mapper */
   Ast_mapper.register "awesoe"
     (fun args => migration.copy_mapper (Ppx_test_lib.TestMapper.mapper args));
 */
/* open Migrate_parsetree.Ast_403; */

/*let foo_config : ref (option string) = ref None

  let set_foo bar = foo_config := Some bar
  let reset_args () = foo_config := None
  */
let reset_args = () => ();

let args =
  [];
    /*("-foo", Arg.String set_foo, "<bar> Foo value to use in the rewriter")*/

/*(* Rewriter implementation *)*/
let my_rewriter = (config, cookies) =>
  /*let foo = match !foo_config with
      | None -> raise (Arg.Bad "-foo is mandatory")
      | Some foo -> foo
    in*/
  PpxMapper.mapper;

/*(* Registration *)*/
/* let () =
  Migrate_parsetree.Driver.register(
    ~name="testre-ppx",
    Migrate_parsetree.Versions.ocaml_406,
    my_rewriter
  ); */

let () = Ast_mapper.register("ppx_test", (_args => PpxMapper.mapper))
