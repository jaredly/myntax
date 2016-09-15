
open PackTypes.Result;
let module P = PackTypes.Parsing;

let optOr a b => {
  switch a {
    | Some a => a
    | None => b
  };
};

let rec getChildByType children needle => {
  switch children {
    | [{typ: Nonlexical typename _, _} as child, ...rest]
    | [{typ: Lexical typename _ _, _} as child, ...rest] when typename == needle => Some child
    | [{typ: Lexical _ _ true, _} as child, ...rest]
    | [{typ: Nonlexical _ true, _} as child, ...rest]
    | [{typ: Iter, _} as child, ...rest] => {
      switch (getChildByType child.children needle) {
        | Some x => Some x
        | None => getChildByType rest needle
      }
    }
    | [_, ...rest] => getChildByType rest needle
    | [] => None
  }
};

let rec getChildrenByType children needle => {
  /* print_endline ("Getting children " ^ needle); */
  switch children {
    | [{typ: Nonlexical typename _, _} as child, ...rest]
    | [{typ: Lexical typename _ _, _} as child, ...rest] when typename == needle => [child, ...(getChildrenByType rest needle)]
    | [{typ: Lexical _ _ true, _} as child, ...rest]
    | [{typ: Nonlexical _ true, _} as child, ...rest]
    | [{typ: Iter, _} as child, ...rest] => {
      List.concat [(getChildrenByType child.children needle), getChildrenByType rest needle]
    }
    | [_, ...rest] => getChildrenByType rest needle
    | [] => []
  }
};


let rec getChild children needle => {
  switch children {
    | [{label: Some label, _} as child, ..._] when label == needle => Some child
    | [{typ: Lexical _ _ true, _} as child, ...rest]
    | [{typ: Nonlexical _ true, _} as child, ...rest]
    | [{typ: Iter, _} as child, ...rest] => {
      switch (getChild child.children needle) {
        | Some x => Some x
        | None => getChild rest needle
      }
    }
    | [_, ...rest] => getChild rest needle
    | [] => None
  }
};

let rec getChildren children needle => {
  /* print_endline ("Getting children " ^ needle); */
  switch children {
    | [{label: Some label, _} as child, ...rest] when label == needle => [child, ...(getChildren rest needle)]
    | [{typ: Lexical _ _ true, _} as child, ...rest]
    | [{typ: Nonlexical _ true, _} as child, ...rest]
    | [{typ: Iter, _} as child, ...rest] => {
      List.concat [(getChildren child.children needle), getChildren rest needle]
    }
    | [_, ...rest] => getChildren rest needle
    | [] => []
  }
};

let getContents result => {
  switch result.typ {
    | Lexical name contents passThrough => contents
    | _ => failwith "Not a lexical"
  }
};

let contentsOrEmpty node => {
  switch node {
    | None => ""
    | Some x => getContents x
  };
};

let maybeContents node => {
  switch node {
    | None => None
    | Some x => Some (getContents x)
  }
};

let unescapeString x => {
  let contents = String.sub x 1 (String.length x - 2);
  if (String.length contents == 1) {
    contents
  } else {
    Scanf.unescaped contents
  }
};

let unescapeChar x => {
  if (String.length x == 1) {
    String.get x 0
  } else {
    String.get (unescapeString x) 0
  }
};

exception ConversionFailure string;

let unwrap opt => {
  switch opt {
    | Some x => x
    | None => raise (ConversionFailure "Unwrapping none")
  }
};

let assertEq one two => {
  if (one != two) {
    raise (ConversionFailure "Assertion error")
  }
};
