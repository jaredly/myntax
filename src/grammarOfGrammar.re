
open PackTypes.Result;
let module P = PackTypes.Parsing;

let debug_type typ => {
  typ |> Json.resultType_to_string;
};

let optOr a b => {
  switch a {
    | Some a => a
    | None => b
  };
};

let rec getChild children needle => {
  /* switch children {
    | [x, ..._] => print_endline ("Child " ^ debug_type x.typ ^ " (looking for) " ^ needle ^ (optOr x.label ""))
    | _ => ()
  }; */
  switch children {
    | [{typ: Iter, _} as child, ...rest] => {
      /* print_endline ("Iter child: " ^ needle); */
      switch (getChild child.children needle) {
        | Some x => Some x
        | None => getChild rest needle
      }
    }
    | [{label: Some label, _} as child, ..._] when label == needle => {
      /* print_endline ("Found label: " ^ needle ^ (debug_type child.typ)); */
      Some child
    }
    | [{typ: Terminal _, _}, ...rest] => {
      getChild rest needle
    }
    | [child, ...rest] => {
      /* print_endline "skipping"; */
      getChild rest needle
    }
    | [] => None
  }
};

let rec getChildren children needle => {
  /* print_endline ("Getting children " ^ needle); */
  switch children {
    | [{label: Some label, _} as child, ...rest] when label == needle => [child, ...(getChildren rest needle)]
    | [{typ: Iter, _} as child, ...rest] => {
      List.concat [(getChildren child.children needle), getChildren rest needle]
    }
    | [_, ...rest] => getChildren rest needle
    | [] => []
  }
};

let getContents result => {
  switch result.typ {
    | Lexical name contents => contents
    | _ => failwith "Not a lexical"
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

let debug result => {
  /* print_endline (Yojson.Safe.to_string (PackTypes.Result.result_to_yojson result)) */
  ()
};

let rec parseItem item => {
  debug item;
  let node = getChild item.children "inner" |> unwrap |> parseInner (getChild item.children "name");
  let node = switch (getChild item.children "neg") {
    | Some _ => P.Not node
    | None => node
  };
  let node = switch (getChild item.children "lexify") {
    | Some _ => P.Lexify node
    | None => node
  };
  let node = switch (getChild item.children "suffix") {
    | None => node
    | Some {typ: Lexical "suffix_star" _, _} => P.Star node None
    | Some {typ: Lexical "suffix_plus" _, _} => P.Plus node None
    | Some {typ: Lexical "suffix_opt" _, _} => P.Optional node None
    | _ => failwith "Unrecognized suffix"
  };
  node
}

and parseInner maybeName inner => {
  let name = maybeContents maybeName;
  switch inner.typ {
    | Nonlexical "ItemInner_nested" => {
      /* print_endline "going deeper"; */
      P.Group (List.map parseItem (getChildren inner.children "nested"))
    }
    | _ => {let child = (List.hd inner.children);
      switch child.typ {
        | Lexical "ident" contents => {
          if (contents == "any") {
            P.Any name
          } else if (contents == "EOF") {
            P.EOF
          } else {
            P.NonTerminal contents name
          }
        }
        | Lexical "string" contents => P.Terminal (unescapeString contents) name
        | Lexical "char" contents => P.Terminal (unescapeString contents) name
        | Lexical "char_range" contents => (
          P.Chars
          ((getChild child.children "start") |> unwrap |> getContents |> unescapeChar)
          ((getChild child.children "end") |> unwrap |> getContents |> unescapeChar)
          name
        )
        | _ => failwith "Unexpected ItemInner_nested"
      }
    }
  };
};

let parseChoice choice => {
  let name = getChild choice.children "name" |> contentsOrEmpty;
  /* print_endline ("Choice name: " ^ name); */
  let comment = getChild choice.children "comment" |> contentsOrEmpty;
  /* print_endline ("Choice comment: " ^ name); */
  (name, comment, (List.map parseItem (getChildren choice.children "children")))
};

let convert (result: result) => {
  /* print_endline "Converting"; */
  assertEq result.typ (Nonlexical "Start");
  (List.map
  (fun rule => {
    if (rule.typ != Nonlexical "Rule_") {
      failwith ("Not a rule?" ^ (PackTypes.Result.resultTypeDescription rule.typ));
    };
    let name = getChild rule.children "name" |> unwrap |> getContents;
    /* print_endline ("Rule " ^ name); */
    let choices = getChildren rule.children "choices";
    (name, {
      P.passThrough: false,
      P.ignoreWhitespace: P.Inherit,
      P.choices: (List.map parseChoice choices),
    })
  })
  (List.hd result.children).children);
};
