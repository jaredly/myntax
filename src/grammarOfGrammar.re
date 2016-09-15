
open PackTypes.Result;
let module P = PackTypes.Parsing;
open ResultUtils;

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
          } else if (contents == "__comment_eol") {
            P.CommentEOL
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
            /* Printf.eprintf "Ignoring decorator %s\n" name; */
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
  }) result.children);
  let firstDecorators = getChildren (List.hd result.children).children "decorators";
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
