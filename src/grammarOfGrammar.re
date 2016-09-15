
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
    | Some {typ: Lexical "suffix_star" _ _, _} => P.Star node None
    | Some {typ: Lexical "suffix_plus" _ _, _} => P.Plus node None
    | Some {typ: Lexical "suffix_opt" _ _, _} => P.Optional node None
    | _ => failwith "Unrecognized suffix"
  };
  node
}

and parseInner maybeName inner => {
  let name = maybeContents maybeName;
  switch inner.typ {
    | Nonlexical "ItemInner_nested" passThrough => {
      /* print_endline "going deeper"; */
      P.Group (List.map parseItem (getChildren inner.children "nested"))
    }
    | _ => {let child = (List.hd inner.children);
      switch child.typ {
        | Lexical "ident" contents passThrough => {
          if (contents == "any") {
            P.Any name
          } else if (contents == "EOF") {
            P.EOF
          } else {
            P.NonTerminal contents name
          }
        }
        | Lexical "string" contents passThrough => P.Terminal (unescapeString contents) name
        | Lexical "char" contents passThrough => P.Terminal (unescapeString contents) name
        | Lexical "char_range" contents passThrough => (
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

type decoratorArg =
  | Bool bool
  | String string
  | Number int;

type parsedDecorator = {name: string, args: list decoratorArg};

let parseDecorator decorator => {
  let name = getChild decorator.children "name" |> unwrap |> getContents;
  let args = (List.map
    (fun arg => {
      switch arg.typ  {
        | Lexical "decarg_bool" contents _ => Bool (contents == "true" ? true : false)
        | Lexical "decarg_string" contents _ => String (Scanf.unescaped (String.sub contents 1 ((String.length contents) - 2)))
        | Lexical "decarg_number" contents _ => Number (int_of_string contents)
        | x => failwith ("Invalid decorator arg" ^ (PackTypes.resultTypeDescription x))
      }
    })
  (getChildren decorator.children "args"));
  {name, args};
};

let convert (result: result) => {
  /* print_endline "Converting"; */
  assertEq result.typ (Nonlexical "Start" false);
  let rules = (List.map
  (fun rule => {
    if (rule.typ != Nonlexical "Rule_" false) {
      failwith ("Not a rule?" ^ (PackTypes.Result.resultTypeDescription rule.typ));
    };
    let decorators = getChildren rule.children "decorators";
    let (newlines, passThrough) = (List.fold_left
      (fun (white, pass) decorator => {
        let {name, args} = parseDecorator decorator;
        switch (name, args) {
          | ("ignoreNewlines", [Bool whether]) => (whether ? P.Yes : P.No, pass)
          | ("ignoreNewlines", []) => (P.Yes, pass)
          | ("passThrough", []) => (white, true)
          | _ => {
            print_endline ("Ignoring decorator " ^ name);
            (white, pass)
          }
        }
      })
      (P.Inherit, false)
      decorators);
    let name = getChild rule.children "name" |> unwrap |> getContents;
    /* print_endline ("Rule " ^ name); */
    let choices = getChildren rule.children "choices";
    (name, {
      P.passThrough: passThrough,
      P.ignoreNewlines: newlines,
      P.choices: (List.map parseChoice choices),
    })
  })
  (List.hd result.children).children);
  let firstDecorators = getChildren (List.hd (List.hd result.children).children).children "decorators";
  let (lineComment, blockComment) = (List.fold_left
    (fun (lineComment, blockComment) decorator => {
      let {name, args} = parseDecorator decorator;
      switch (name, args) {
        | ("lineComment", [String line]) => (Some line, blockComment)
        | ("blockComment", [String first, String last]) => (lineComment, Some (first, last))
        | _ => (lineComment, blockComment)
      }
    })
    (None, None)
    firstDecorators);
  let open PackTypes.Parsing;
  {lineComment, blockComment, rules}
};
