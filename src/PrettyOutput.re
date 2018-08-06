module Output = {

  /*** TODO how do I whitespace? newlined things can be indented, yes, but how about whitespace between things? **/

  type outputT =
    | Text(string)
    | EOL
    | NoSpace
    | MaybeNewlined(list(outputT))
    | Newlined(list(outputT))
    | Lexical(list(outputT))
    | Straight(list(outputT));
};

type config = {
  maxWidth: int,
  indentWidth: int,
  indentStr: string
};

let pad = (num, base) => {
  let txt = ref("");
  for (i in 0 to num) {
    txt := txt^ ++ base
  };
  txt^
};

type iterim =
  | Text(string)
  | NoSpace;

let rec outputToString = (config, indentLevel, output) =>
  switch output {
  | Output.EOL => ("\n" ++ pad(indentLevel - 1, config.indentStr), true) /* TODO need to account for current indent level too */
  | Output.NoSpace => failwith("unhandled nospace")
  /* ("", false) /* TODO make sure this isn't happening much... */ */
  /* failwith "NoSpace should be handled by the parent" */
  | Output.Text(str) => (str, false) /* TODO check for newlines? */
  /*** TODO multiline strings -- should I be aware of that? Also lexical things that span multiple lines.. for some reason */
  | Output.Lexical(items) => (
      String.concat("", List.map((x) => fst(outputToString(config, 0, x)), items)),
      false
    )
  | Output.Straight(items) =>
    let rec loop = (items) =>
      switch items {
      | [] => ("", false)
      | [Output.NoSpace, ...rest] => loop(rest)
      | [child] => outputToString(config, indentLevel, child)
      | [child, Output.NoSpace, ...rest] =>
        let (restext, multi) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel, child);
        (res ++ restext, multi || nmulti)
      | [child, ...rest] =>
        let (restext, multi) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel, child);
        (res ++ (" " ++ restext), multi || nmulti)
      };
    loop(items)
  | Output.MaybeNewlined(items) =>
    let rec loop = (items) =>
      switch items {
      | [] => ([], 0, 0)
      | [Output.NoSpace, ...rest] =>
        let (items, len, multis) = loop(rest);
        /* let (res, nmulti) = outputToString config indentLevel child; */
        ([NoSpace, ...items], len, multis)
      | [child, ...rest] =>
        let (items, len, multis) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel + 1, child);
        ([Text(res), ...items], len + String.length(res), nmulti ? multis + 1 : multis)
      };
    let (items, total, multis) = loop(items);
    if (multis > 1
        || total
        + config.indentWidth
        * indentLevel > config.maxWidth
        && List.length(items) > 1) {
      let padt = "\n" ++ pad(indentLevel + 0, config.indentStr);
      let rec loop = (items) =>
        switch items {
        | [] => ""
        | [Text(child)] => child
        | [Text(child), NoSpace, ...rest] => child ++ loop(rest)
        | [Text(child), ...rest] => child ++ (padt ++ loop(rest))
        | [NoSpace, ...rest] => loop(rest)
        };
      (padt ++ (loop(items) ++ ("\n" ++ pad(indentLevel - 1, config.indentStr))), true)
    } else {
      let rec loop = (items) =>
        switch items {
        | [] => ""
        | [Text(child)] => child
        | [Text(child), NoSpace, ...rest] => child ++ loop(rest)
        | [Text(child), ...rest] => child ++ (" " ++ loop(rest))
        | [NoSpace, ...rest] => loop(rest)
        };
      (loop(items), false)
    }
  | Output.Newlined(items) =>
    let padt = "\n" ++ pad(indentLevel + 0, config.indentStr);
    let rec loop = (items) =>
      switch items {
      | [] => ("", false)
      | [Output.NoSpace, ...rest] => loop(rest)
      | [child] => outputToString(config, indentLevel + 1, child)
      | [child, Output.NoSpace, ...rest] =>
        let (restext, multi) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel + 1, child);
        (res ++ restext, multi || nmulti)
      | [child, ...rest] =>
        let (restext, multi) = loop(rest);
        let (res, nmulti) = outputToString(config, indentLevel + 1, child);
        (res ++ (padt ++ restext), multi || nmulti)
      };
    let (str, multi) = loop(items);
    (padt ++ (str ++ ("\n" ++ pad(indentLevel - 1, config.indentStr))), multi)
  };
