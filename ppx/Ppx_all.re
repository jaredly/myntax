
      /* let monads = Ppx_Monads.mapper();
      let test = PpxMapper.mapper;
      let items = monads.structure(monads, items);
      let items = test.structure(test, items); */

let () = Ast_mapper.run_main((_) => CompositeMapper.make([Ppx_Monads.mapper, PpxTest.mapper, Ppx_Grammar.mapper]));