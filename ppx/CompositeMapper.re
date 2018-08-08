
let make = mappers => {
  ...Ast_mapper.default_mapper,
  structure: (mapper, items) => {
    let items = List.fold_left((items, mapper) => mapper.Ast_mapper.structure(mapper, items), items, mappers);
    Ast_mapper.default_mapper.structure(mapper, items);
  }
};
