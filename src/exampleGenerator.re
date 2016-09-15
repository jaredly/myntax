
let module P = PackTypes.Parsing;
let module R = PackTypes.Result;

let memo: Hashtbl.t string string = Hashtbl.create 100;

let mLoc = (0,0);

let optOr orr opt => switch opt { | None => orr | Some x => x };

let maybePrint grammar result => {
  try {
    PrettyPrint.toString grammar result
  } {
    | Failure message => Some message
  }
};

let rec generateForItem grammar table depth item => {
  switch item {
    | P.NonTerminal name label => [((optOr "" label), generateForRule grammar table name (depth + 1))]
    /* [((optOr "" label), R.Leaf (name, "") ("<" ^ name ^ ">") mLoc)] */
    | P.Terminal contents label => switch label {
      | Some label => [(label, R.Leaf ("", "") contents mLoc)]
      | None => []
    }

    | P.Lexify p
    | P.NoSpaceAfter p
    | P.NoSpaceBefore p
    | P.Star p
    | P.Plus p
    | P.Optional p => generateForItem grammar table depth p

    | P.Group p => List.concat (List.map (generateForItem grammar table depth) p)

    | P.Not _
    | P.Any _
    | P.Lookahead _
    | P.EOF
    | P.Empty
    | P.CommentEOL => []

    | P.Chars start cend label => {
      let s = (Char.code start);
      [((optOr "" label), R.Leaf ("", "") (Printf.sprintf "%c" (Char.chr ((Random.int ((Char.code cend) - s)) + s))) mLoc)]
    }
  }
}

and generateForRule grammar table rulename depth => {
  try {
    R.Leaf (rulename, "") (Hashtbl.find table rulename) mLoc
  } {
    | Not_found => {
      if (depth > 3) {
        R.Leaf (rulename, "") ("<" ^ rulename ^ ">") mLoc
      } else {
        let rule = List.assoc rulename grammar.P.rules;
        let choice = Random.int (List.length rule.P.choices);
        let (sub, _, items) = List.nth rule.P.choices choice;
        R.Node (rulename, sub) (List.concat (List.map (generateForItem grammar table depth) items )) mLoc
      }
    }
  };
};

let generateForChoice grammar table rule items => {
  R.Node rule (List.concat (List.map (generateForItem grammar table 0) items)) mLoc
};

let generateExamples grammar ruleName table => {
  let {P.choices, _} = List.assoc ruleName grammar.P.rules;
  (
    List.map
    (fun (sub, comment, items) => {
      sub ^ ":\n" ^ (generateForChoice grammar table (ruleName, sub) items |> maybePrint grammar |> optOr "Got nothing while printing")
    })
    choices
  ) |> String.concat "\n"
};

/* let generateExamples grammar => {
  (
    List.map
    (fun (name, rule) => {
      List.map
      (fun (sub, comment, items) => {
        name ^ " - " ^ sub ^ ":  " ^ (generateForRule (name, sub) items |> maybePrint grammar |> optOr "Unable to pretty print")
      })
      rule.P.choices
    })
    grammar.P.rules
  ) |> List.concat |> String.concat "\n"
}; */
