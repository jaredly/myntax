
let () = Ast_mapper.run_main((_) => CompositeMapper.make([Ppx_Monads.mapper, PpxTest.mapper, Ppx_Grammar.mapper]));