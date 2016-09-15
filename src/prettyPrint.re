
open PackTypes.Result;
open PackTypes.Parsing;


let rec toString grammar {typ, children} => {
  switch typ {
    | Terminal value => value
    | Lexical _ contents _ => contents
    | Nonlexical (name, sub, index) _ => {
      let {ignoreNewlines, choices, passThrough} = List.assoc name grammar;
      let (_, _, items) = List.nth choices index;
      /* String.concat "" (List.map (toString grammar) children) */
      String.concat "" (itemsToString grammar items children)
    }
  }
}
and itemsToString grammar items children => {
  switch items {
    | [Terminal text _, ...rest] => [text, ...(itemsToString grammar rest children)]
    | [] => []
    | _ => []
  }
};
