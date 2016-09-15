
open PackTypes.Result;
open PackTypes.Parsing;
let module P = PackTypes.Parsing;

let module Output = {
  /** TODO how do I whitespace? newlined things can be indented, yes, but how about whitespace between things? **/
  /** I'm imagining maybe a rule annotation that says "everything has a splace between them" or sth. And then maybe
  a pseudo-item that says "suppress space here" or "add space here" **/
  type outputT =
    | Text string
    | EOL
    | NoSpace
    | MaybeNewlined (list outputT)
    | Lexical (list outputT)
    | Straight (list outputT)
    /* | Newlined (list outputT) TODO I do think I want this... */
    [@@deriving show];
};

type config = {
  maxWidth: int,
  indentWidth: int,
  indentStr: string,
};

let pad num base => {
  let txt = ref "";
  for i in 0 to num {
    txt := !txt ^ base;
  };
  !txt
};

type iterim =
  | Text string
  | NoSpace;

let rec outputToString config indentLevel output => {
  switch output {
    | Output.EOL => ("\n" ^ (pad (indentLevel - 1) config.indentStr), true) /* TODO need to account for current indent level too */
    | Output.NoSpace => ("NOSPACE", false)
    /* failwith "NoSpace should be handled by the parent" */
    | Output.Text str => (str, false) /* TODO check for newlines? */
    /** TODO multiline strings -- should I be aware of that? Also lexical things that span multiple lines.. for some reason */
    | Output.Lexical items => (String.concat "" (List.map (fun x => fst (outputToString config 0 x)) items), false)
    | Output.Straight items => {
      let rec loop items => {
        switch items {
          | [] => ("", false)
          | [child] => outputToString config indentLevel child
          | [child, Output.NoSpace, ...rest] => {
            let (restext, multi) = loop rest;
            let (res, nmulti) = outputToString config indentLevel child;
            (res ^ restext, multi || nmulti)
          }
          | [child, ...rest] => {
            let (restext, multi) = loop rest;
            let (res, nmulti) = outputToString config indentLevel child;
            (res ^ " " ^ restext, multi || nmulti)
          }
        }
      };

      loop items;
    }
    | Output.MaybeNewlined items => {

      let rec loop items => {
        switch items {
          | [] => ([], 0, 0)
          | [Output.NoSpace, ...rest] => {
            let (items, len, multis) = loop rest;
            /* let (res, nmulti) = outputToString config indentLevel child; */
            ([NoSpace, ...items], len, multis)
          }
          | [child, ...rest] => {
            let (items, len, multis) = loop rest;
            let (res, nmulti) = outputToString config (indentLevel + 1) child;
            ([Text res, ...items], len + String.length res, nmulti ? multis + 1 : multis)
          }
        }
      };

      let (items, total, multis) = loop items;
      if (multis > 1 || total + (config.indentWidth * indentLevel) > config.maxWidth && List.length items > 1) {
        let padt = "\n" ^ (pad (indentLevel + 0) config.indentStr);
        let rec loop items => {
          switch items {
            | [] => ""
            | [Text child] => child
            | [Text child, NoSpace, ...rest] => {
              child ^ (loop rest)
            }
            | [Text child, ...rest] => {
              child ^ padt ^ (loop rest)
            }
            | [NoSpace, ...rest] => loop rest
          }
        };
        /* let txt = padt ^ (String.concat (padt) items) ^ "\n"; */
        (padt ^ (loop items) ^ "\n" ^ (pad (indentLevel - 1) config.indentStr), true)
      } else {
        /* (String.concat " " items, false) */
        let rec loop items => {
          switch items {
            | [] => ""
            | [Text child] => child
            | [Text child, NoSpace, ...rest] => {
              child ^ (loop rest)
            }
            | [Text child, ...rest] => {
              child ^ " " ^ (loop rest)
            }
            | [NoSpace, ...rest] => loop rest
          }
        };
        (loop items, false)
      }
    }
  }
};

/* Finds an item and returns the list without that item */
let rec maybeFind children check => {
  switch children {
    | [] => (None, [])
    | [child, ...rest] => switch (check child) {
      | None => {
        let (res, rest) = maybeFind rest check;
        (res, [child, ...rest])
      }
      | x => (x, rest)
    }
  }
};

let findByLabel children needle => {
  maybeFind children (fun (label, child) => label == needle ? Some child : None)
};

/* let findNodeByType children needle => {
  maybeFind children (fun (_, child) => {
    switch child {
      | Leaf _ => None
      | Node (name, sub) children _ as child when name == needle => Some ((name, sub), children)
    }
  })
}; */

let findByType children needle => {
  maybeFind children (fun (_, child) => {
    switch child {
      | Leaf (name, sub) _ _ as child
      | Node (name, sub) _ _ as child when name == needle => Some child
      | _ => None
    }
  })
};

let rec greedy loop p children min max => {
  Printf.printf "Greedy %d %d\n" min max;
  if (max == 0) {
    (true, [], children)
  } else {
    let (success, res, unused) = loop [p] children;
    if (not success) {
      print_endline ("First greed aborted, and " ^ (min <= 0 ? "yup" : "nop"));
      (min <= 0, [], children)
    } else {
      let (s2, r2, u2) = greedy loop p unused (min - 1) (max - 1);
      print_endline ("Inner greed " ^ (s2 ? "yup" : "nop"));
      if (s2) {
        (true, List.concat [res, r2], u2)
      } else {
        if (min <= 1) {
          (true, res, unused)
        } else {
          (false, [], children)
        }
      }
    }
  }
};

let passThroughChildren grammar name => {
  let rule = List.assoc name grammar.rules;
  if (rule.passThrough) {
    let (a, b, c) = (List.hd rule.choices); /** TODO test multiple? */
    Some (c, rule.ignoreNewlines)
  } else {
    None
  }
};

let rec resultToOutput: bool => grammar => result => option Output.outputT = fun ignoringNewlines grammar result => {
  switch result {
    | Leaf _ contents _ => Some (Output.Text contents)
    | Node (name, sub) children _ => nodeToOutput ignoringNewlines grammar (name, sub) children
  }
}

and nodeToOutput ignoringNewlines grammar (name, sub) children => {
  Printf.printf "Output: %s %s\n" name sub;
  let rule = List.assoc name grammar.rules;
  let (_, _, items) = List.find (fun (name, _ ,_) => name == sub) rule.choices;
  let ignoringNewlines = switch (rule.ignoreNewlines, ignoringNewlines) {
    | (Yes, _) => true
    | (No, _) => false
    | (Inherit, x) => x
  };
  let isLexical = (Char.uppercase (String.get name 0)) != (String.get name 0);
  print_endline ("Ignoring newlines: " ^ (ignoringNewlines ? "yep" :" nop"));

  let rec loop ignoringNewlines items children => {

    switch items {
      | [] => (true, [], children)
      | [item, ...rest] => {
        switch item {
          | Terminal text None => {
            let (success, res, unused) = (loop ignoringNewlines rest children);
            (success, [Output.Text text, ...res], unused)
          }
          | Terminal text (Some label) => {
            switch (findByLabel children label) {
              | (None, _) => (print_endline ("unable to find by label " ^ label));(false, [], children)
              | (Some x, children) => {
                let (success, res, unused) = (loop ignoringNewlines rest children);
                (success, [Output.Text text, ...res], unused)
              }
            }
          }

          | Any _
          | Chars _ => failwith "Chars shouldn't be at the top level"

          | NonTerminal name label => {
            switch (passThroughChildren grammar name) {
              | Some (subs, ignoreNewlines) => {
                /* print_endline "passthrough"; */
                let newIgnore = switch (ignoreNewlines, ignoringNewlines) {
                  | (Yes, _) => true
                  | (No, _) => false
                  | (Inherit, x) => x
                };
                let (success, res, unused) = loop newIgnore subs children;
                if (not success) {
                  (false, [], children)
                } else {
                  let (s2, r2, u2) = loop ignoringNewlines rest unused;
                  let output = switch res {
                    | [sub] => sub
                    | res => if (ignoreNewlines == Yes) {
                      Output.MaybeNewlined res
                    } else {
                      Output.Straight res
                    }
                  };
                  (s2, [output, ...r2], u2)
                }
                /* loop (List.concat [subs, rest]) children */
              }
              | None => {
                let (child, others) = switch label {
                  | Some label => switch (findByLabel children label) {
                    | (None, _) => findByType children name
                    | x => x
                  }
                  | None => findByType children name
                };
                switch child {
                  | None => {print_endline ("no child "^name);(false, [], children)}
                  | Some result => switch (resultToOutput ignoringNewlines grammar result)  {
                    | None => print_endline ("no child "^name);(false, [], children)
                    | Some output => {
                      let (success, res, unused) = loop ignoringNewlines rest others;
                      (success, [output, ...res], unused)
                    }
                  }
                }
              }
            }
          }

          | Lexify p => loop ignoringNewlines [p, ...rest] children

          | NoSpaceAfter p => {
            let (success, res, unused) = loop ignoringNewlines [p] children;
            if (success) {
              let (s2, r2, u2) = loop ignoringNewlines rest unused;
              (s2, List.concat [res, [Output.NoSpace], r2], u2)
            } else {
              (success, res, unused)
            }
          }
          | NoSpaceBefore p => {
            let (success, res, unused) = loop ignoringNewlines [p] children;
            if (success) {
              let (s2, r2, u2) = loop ignoringNewlines rest unused;
              (s2, List.concat [[Output.NoSpace], res, r2], u2)
            } else {
              (success, res, unused)
            }
          }

          | Group p => {
            let (success, res, unused) = loop ignoringNewlines p children;
            if success {
              let (s2, r2, u2) = loop ignoringNewlines rest unused;
              (s2, List.concat [res, r2], u2)
            } else {
              (success, res, unused)
            }
          }

          | CommentEOL /* TODO print comments back */
          => {
            let (success, res, unused) = loop ignoringNewlines rest children;
            (success, [Output.EOL, ...res], unused)
          }

          | EOF
          | Empty
          | Lookahead _
          | Not _ => loop ignoringNewlines rest children

          | Star p _ => {
            let (success, res, unused) = greedy (loop ignoringNewlines) p children 0 (-1);
            if success {
              let (s2, r2, u2) = loop ignoringNewlines rest unused;
              (s2, List.concat [res, r2], u2)
            } else {
              (success, res, unused)
            }
          }
          | Plus p _ => {
            let (success, res, unused) = greedy (loop ignoringNewlines) p children 1 (-1);
            if success {
              let (s2, r2, u2) = loop ignoringNewlines rest unused;
              (s2, List.concat [res, r2], u2)
            } else {
              (success, res, unused)
            }
          }
          | Optional p _ => {
            let (success, res, unused) = greedy (loop ignoringNewlines) p children 0 1;
            if success {
              let (s2, r2, u2) = loop ignoringNewlines rest unused;
              (s2, List.concat [res, r2], u2)
            } else {
              (success, res, unused)
            }
          }
        }
      }
    };
  };

  let (success, res, unused) = (loop ignoringNewlines items children);
  switch unused {
    | [] => Some (switch res {
      | [sub] => sub
      | _ => if (isLexical) {
          Lexical res
      } else if (rule.ignoreNewlines == Yes) {
          MaybeNewlined res
        } else {
          Straight res
        }
      })
    | _ => {
      print_endline "Some unused";
      None
    }
  }
}
;

let toString (grammar: grammar) result => {
  switch (resultToOutput false grammar result) {
    | Some output => {
      print_endline (Output.show_outputT output);
        Some (fst (outputToString {
        indentWidth: 2,
        indentStr: ". ",
        maxWidth: 50
      } 0 output))
    }
    | None => None
  }
};
