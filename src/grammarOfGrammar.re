
open PackTypes.Result;
let module P = PackTypes.Parsing;
/* let module NP = PackTypes.NewParsing; */
let module RU = ResultUtils;
/* let getChild = ResultUtils.getChild;
let getChildren = ResultUtils.getChildren;
let getContentsByLabel = ResultUtils.getContentsByLabel;
let getContentsByType = ResultUtils.getContentsByType;
let getPres = ResultUtils.getContentsByType;
let unwrap = ResultUtils.unwrap; */

type decoratorArg =
  | Bool bool
  | String string
  | Number int;

let parseString contents => (String.sub contents 1 ((String.length contents) - 2)) |> Scanf.unescaped;

let parseDecorator children => {
  let name = RU.getContentsByLabel children "name" |> RU.unwrap;
  let args = RU.getChildren children (fun child => {
    switch child {
      | ("args", Node ("decarg", sub) children _) => {
        Some (switch sub {
          | "bool" => Bool ((RU.unwrap (RU.getContentsByType children "bool")) == "true" ? true : false)
          | "string" => String (parseString (RU.unwrap (RU.getContentsByType children "string")))
          | "number" => Number (int_of_string (RU.unwrap (RU.getContentsByType children "number")))
          | _ => failwith "unexpected arg type"
        })
      }
      | _ => None
    }
  });
  (name, args);
};

let optOr orr opt => switch opt {
  | Some x => x
  | None => orr
};

let getFlag children => RU.getChild children (fun child => {
  switch child {
    | ("flag", Node (_, sub) _ _) => Some sub
    | ("flag", _) => failwith "Flag expected to be non-leaf"
    | _ => None
  }
});

let unescapeChar txt => (String.get (Scanf.unescaped txt) 0);
let unescapeString = Scanf.unescaped;
let isSome x => switch x {
  | Some _ => true
  | None => false
};

let getSuffix children => RU.getChild children (fun child => {
  switch child {
    | ("suffix", Node (_, sub) _ _) => Some sub
    | ("suffix", _) => failwith "Suffix expected to be non-leaf"
    | _ => None
  }
});

let rec parseInner label ((_, sub), children, loc) => {
  if (sub == "nested") {
    if (isSome label) {
      failwith ("groups can't have labels: " ^ (RU.unwrap label))
    };
    P.Group (RU.getChildren children (fun (label, child) => {
      if (label == "nested") {
        switch child {
          | Node _ children _ => Some (parseItem children)
          | _ => failwith "Nested child expected to be non-leaf"
        }
      } else {
        None
      }
    }))
  } else {
    RU.getChild children (fun (_, child) => {
      switch child {
        | Leaf ("string", _) contents _ => Some (P.Terminal contents label)
        | Leaf ("ident", _) contents _ => Some (P.NonTerminal contents label)
        | Node ("char", _) children _ => RU.getChild children (fun (_, child) => switch child {
            | Leaf ("single", _) contents _ => Some (P.Terminal (unescapeString contents) label)
            | _ => None
          })
        | Node ("char_range", _) children _ => {
          let start = RU.getContentsByLabel children "start" |> RU.unwrap;
          let send = RU.getContentsByLabel children "end" |> RU.unwrap;
          Some (P.Chars (unescapeChar start) (unescapeChar send) label)
        }
        | _ => None
      }
    }) |> RU.unwrap
  }
}

and parseItem children => {
  let neg = RU.getPresenceByLabel children "neg";
  let lexify = RU.getPresenceByLabel children "lexify";
  let suffix = getSuffix children;
  let _ = getFlag children; /* TODO use flags? */
  let label = RU.getContentsByLabel children "name";
  let inner = RU.getNodeByLabel children "inner" |> RU.unwrap |> parseInner label;

  let inner = switch suffix {
    | None => inner
    | Some "plus" => P.Plus inner None
    | Some "star" => P.Star inner None
    | Some "opt" => P.Optional inner None
    | _ => failwith "unexpected suffix"
  };
  let inner = neg ? P.Not inner : inner;
  let inner = lexify ? P.Lexify inner : inner;
  inner
};

let parseChoice children => {
  let name = RU.getContentsByLabel children "name" |> optOr "";
  let comment = RU.getContentsByLabel children "comment" |> optOr "";
  let children = RU.getChildren children (fun (label, child) => {
    if (label != "children") {
      None
    } else {
      switch child {
        | Leaf _ => failwith "Invalid child"
        | Node _ children _ => Some (parseItem children)
      }
    }
  });
  (name, comment, children)
};

let parseRule children => {
  let name = RU.getContentsByLabel children "name" |> RU.unwrap;
  let (newLines, passThrough, leaf) = List.fold_left
  (fun flags child => switch child {
    | ("decorators", Node _ children _) => {
      let (white, pass, leaf) = flags;
      switch (parseDecorator children) {
        | ("ignoreNewlines", [Bool whether]) => (whether ? P.Yes : P.No, pass, leaf)
        | ("ignoreNewlines", []) => (P.Yes, pass, leaf)
        | ("passThrough", []) => (white, true, leaf)
        | ("leaf", []) => (white, pass, true)
        | ("lineComment", _)
        | ("blockComment", _) => flags
        | (name, _) => {
          Printf.eprintf "Ignoring decorator %s\n" name;
          flags
        }
      }
    }
    | _ => flags
  })
  (P.Inherit, false, false)
  children
  ;

  (name, {
    P.passThrough: passThrough,
    P.ignoreNewlines: newLines,
    P.leaf: leaf,
    P.choices: RU.getChildren children (fun (_, child) => switch child {
      | Node ("Choice", _) children _ => Some (parseChoice children)
      | _ => None
    }),
  })
};

let getToplevelDecorators children => {
  List.fold_left (fun decs child => switch child {
    | ("decorators", Node _ children _) => {
      let (line, block) = decs;
      switch (parseDecorator children) {
        | ("lineComment", [String line]) => (Some line, block)
        | ("blockComment", [String one, String two]) => (line, Some (one, two))
        | _ => decs
      }
    }
    | _ => decs
  })
  (None, None)
  children
};

let convert (result: result) => {
  switch result {
    | Node ("Start", _) children _ => {
      let rules = RU.getChildren children (
        fun (_, child) => switch child {
          | Node ("Rule", _) children _ => Some (parseRule children)
          | _ => None
        }
      );
      let (lineComment, blockComment) = RU.getChild children (fun (label, child) => {
        switch child {
          | Node ("Rule", _) children _ => Some (getToplevelDecorators children)
          | _ => None
        }
      }) |> optOr (None, None);
      {P.lineComment, blockComment, rules}
    }
    | _ => failwith "Base must be of type `start`"
  };
};

/* let rec parseItem item => {
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
    | Some {typ: Lexical ("suffix", "star", _) _ _, _} => P.Star node None
    | Some {typ: Lexical ("suffix", "plus", _) _ _, _} => P.Plus node None
    | Some {typ: Lexical ("suffix", "opt", _) _ _, _} => P.Optional node None
    | _ => failwith "Unrecognized suffix"
  };
  node
}

and parseInner maybeName inner => {
  let name = maybeContents maybeName;
  switch inner.typ {
    | Nonlexical ("ItemInner", "nested", _) passThrough => {
      /* print_endline "going deeper"; */
      P.Group (List.map parseItem (getChildren inner.children "nested"))
    }
    | _ => {let child = (List.hd inner.children);
      switch child.typ {
        | Lexical ("ident", _, _) contents passThrough => {
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
        | Lexical ("string", _, _) contents passThrough => P.Terminal (unescapeString contents) name
        | Lexical ("char", "", _) contents passThrough => P.Terminal (unescapeString contents) name
        | Lexical ("char_range", "", _) contents passThrough => (
          P.Chars
          ((getChild child.children "start") |> unwrap |> getContents |> unescapeChar)
          ((getChild child.children "end") |> unwrap |> getContents |> unescapeChar)
          name
        )
        | _ => {
          print_endline (PackTypes.show_result inner);
          failwith "Unexpected ItemInner_nested"
        }
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
        | Lexical ("decarg", "bool", _) contents _ => Bool (contents == "true" ? true : false)
        | Lexical ("decarg", "string", _) contents _ => String (Scanf.unescaped (String.sub contents 1 ((String.length contents) - 2)))
        | Lexical ("decarg", "number", _) contents _ => Number (int_of_string contents)
        | x => failwith ("Invalid decorator arg" ^ (PackTypes.resultTypeDescription x))
      }
    })
  (getChildren decorator.children "args"));
  {name, args};
}; */

/* let convert (result: result) => {
  /* print_endline "Converting"; */
  /* assertEq result.typ (Nonlexical ("Start", "", _) false); */
  let rules = (List.map
  (fun rule => {
    switch rule {
      | Node ("Rule", _) children _ => {
        let rec loop children acc => {
          switch children {
            [] => acc
            [(_, Leaf _ _ _), ...rest] => loop rest acc
            [("decorators", Node (_, argtype) _ _), ...rest] =>
          }
        }

        let decorators = getChildren children "decorators";
        let (newlines, passThrough, leaf) = (List.fold_left
          (fun (white, pass, leaf) decorator => {
            let {name, args} = parseDecorator decorator;
            switch (name, args) {
              | ("ignoreNewlines", [Bool whether]) => (whether ? P.Yes : P.No, pass, leaf)
              | ("ignoreNewlines", []) => (P.Yes, pass, leaf)
              | ("passThrough", []) => (white, true, leaf)
              | ("leaf", []) => (white, pass, true)
              | _ => {
                /* Printf.eprintf "Ignoring decorator %s\n" name; */
                (white, pass, leaf)
              }
            }
          })
          (P.Inherit, false, false)
          decorators);
        let name = getChild rule.children "name" |> unwrap |> getContents;
        /* print_endline ("Rule " ^ name); */
        let choices = getChildren rule.children "choices";
        (name, {
          P.passThrough: passThrough,
          P.ignoreNewlines: newlines,
          P.leaf: leaf,
          P.choices: (List.map parseChoice choices),
        })

      }
      | _ => failwith ("Not a rule?" ^ (PackTypes.Result.resultTypeDescription rule.typ));
    }
    /* switch rule.typ {
      | Nonlexical ("Rule", "", _) _ => ()
      | _ => failwith ("Not a rule?" ^ (PackTypes.Result.resultTypeDescription rule.typ));
    }; */
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
}; */
