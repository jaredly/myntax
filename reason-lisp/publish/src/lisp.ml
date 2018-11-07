module PackTypes
= struct
#1 "packTypes.ml"
[@@@ocaml.ppx.context { cookies = [] }]
module Parsing =
  struct
    type ignoreNewlines =
      | Yes
      | No
      | Inherit[@@deriving show]
    type grammar =
      {
      lineComment: string option;
      blockComment: (string* string) option;
      rules: (string* rule) list;}
    and rule =
      {
      passThrough: bool;
      ignoreNewlines: ignoreNewlines;
      capturesComments: bool;
      preserveInnerSpace: bool;
      leaf: bool;
      docs: string option;
      choices: choice list;}
    and choice = (string* string* parsing list)
    and parsing =
      | Star of parsing
      | Plus of parsing
      | Optional of parsing
      | Lookahead of parsing
      | Not of parsing
      | Lexify of parsing
      | Indent
      | FullIndent
      | NoSpaceAfter of parsing
      | NoSpaceBefore of parsing
      | NoBreakBefore of parsing
      | NoBreakAfter of parsing
      | Any of string option
      | EOF
      | CommentEOL
      | Group of parsing list
      | NonTerminal of string* string option
      | Terminal of string* string option
      | Chars of char* char* string option
      | Empty[@@deriving show]
    let showIgnore i =
      match i with
      | Yes  -> (("Yes")[@reason.raw_literal "Yes"])
      | No  -> (("No")[@reason.raw_literal "No"])
      | Inherit  -> (("Inherit")[@reason.raw_literal "Inherit"])
    let showOption o v =
      match o with
      | None  -> (("None")[@reason.raw_literal "None"])
      | ((Some (x))) ->
          (("Some(")[@reason.raw_literal "Some("]) ^
            ((v x) ^ ((")")[@reason.raw_literal ")"]))
    let showString s = Printf.sprintf (("%S")[@reason.raw_literal "%S"]) s
    let showChar c =
      (("'")[@reason.raw_literal "'"]) ^
        ((Char.escaped c) ^ (("'")[@reason.raw_literal "'"]))
    let showBool t =
      match t with
      | true  -> (("true")[@reason.raw_literal "true"])
      | false  -> (("false")[@reason.raw_literal "false"])
    let showList ?(oneline= true)  items show =
      match oneline with
      | true  ->
          (("[")[@reason.raw_literal "["]) ^
            ((String.concat ((", ")[@reason.raw_literal ", "])
                (List.map show items))
               ^ (("]")[@reason.raw_literal "]"]))
      | false  ->
          (("[\n")[@reason.raw_literal "[\\n"]) ^
            ((String.concat ((",\n")[@reason.raw_literal ",\\n"])
                (List.map show items))
               ^ (("\n]")[@reason.raw_literal "\\n]"]))
    let rec showParsing parsing =
      match parsing with
      | ((Any (inner))) ->
          (("Any(")[@reason.raw_literal "Any("]) ^
            ((showOption inner showString) ^ ((")")[@reason.raw_literal ")"]))
      | ((Star (p))) ->
          (("Star(")[@reason.raw_literal "Star("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((Plus (p))) ->
          (("Plus(")[@reason.raw_literal "Plus("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((Optional (p))) ->
          (("Optional(")[@reason.raw_literal "Optional("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((NoSpaceAfter (p))) ->
          (("NoSpaceAfter(")[@reason.raw_literal "NoSpaceAfter("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((NoSpaceBefore (p))) ->
          (("NoSpaceBefore(")[@reason.raw_literal "NoSpaceBefore("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((NoBreakAfter (p))) ->
          (("NoBreakAfter(")[@reason.raw_literal "NoBreakAfter("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((NoBreakBefore (p))) ->
          (("NoBreakBefore(")[@reason.raw_literal "NoBreakBefore("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((Lookahead (p))) ->
          (("Lookahead(")[@reason.raw_literal "Lookahead("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((Not (p))) ->
          (("Not(")[@reason.raw_literal "Not("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | ((Lexify (p))) ->
          (("Lexify(")[@reason.raw_literal "Lexify("]) ^
            ((showParsing p) ^ ((")")[@reason.raw_literal ")"]))
      | EOF  -> (("EOF")[@reason.raw_literal "EOF"])
      | Indent  -> (("Indent")[@reason.raw_literal "Indent"])
      | FullIndent  -> (("FullIndent")[@reason.raw_literal "FullIndent"])
      | CommentEOL  -> (("CommentEOL")[@reason.raw_literal "CommentEOL"])
      | ((Group (inner))) ->
          (("Group(")[@reason.raw_literal "Group("]) ^
            ((showList inner showParsing) ^ ((")")[@reason.raw_literal ")"]))
      | ((NonTerminal (name,label))) ->
          (("NonTerminal(")[@reason.raw_literal "NonTerminal("]) ^
            ((showString name) ^
               (((", ")[@reason.raw_literal ", "]) ^
                  ((showOption label showString) ^
                     ((")")[@reason.raw_literal ")"]))))
      | ((Terminal (name,label))) ->
          (("Terminal(")[@reason.raw_literal "Terminal("]) ^
            ((showString name) ^
               (((", ")[@reason.raw_literal ", "]) ^
                  ((showOption label showString) ^
                     ((")")[@reason.raw_literal ")"]))))
      | Empty  -> (("Empty")[@reason.raw_literal "Empty"])
      | ((Chars (startC,endC,label))) ->
          (("Chars(")[@reason.raw_literal "Chars("]) ^
            ((showChar startC) ^
               (((", ")[@reason.raw_literal ", "]) ^
                  ((showChar endC) ^
                     (((", ")[@reason.raw_literal ", "]) ^
                        ((showOption label showString) ^
                           ((")")[@reason.raw_literal ")"]))))))
    let showChoice (name,comment,parsings) =
      (("(")[@reason.raw_literal "("]) ^
        ((showString name) ^
           (((", ")[@reason.raw_literal ", "]) ^
              ((showString comment) ^
                 (((", ")[@reason.raw_literal ", "]) ^
                    ((showList parsings showParsing) ^
                       ((")")[@reason.raw_literal ")"]))))))
    let showRule { passThrough; ignoreNewlines; leaf; docs; choices } =
      Printf.sprintf
        {|{
    passThrough: %s,
    ignoreNewlines: %s,
    capturesComments: false,
    preserveInnerSpace: false,
    leaf: %s,
    docs: %s,
    choices: %s,
  }|}
        (showBool passThrough) (showIgnore ignoreNewlines) (showBool leaf)
        (showOption docs showString)
        (showList ~oneline:((List.length choices) = 1) choices showChoice)
    let showGrammar { lineComment; blockComment; rules } =
      Printf.sprintf
        (("{lineComment: %s, blockComment: %s, rules: %s}")[@reason.raw_literal
                                                             "{lineComment: %s, blockComment: %s, rules: %s}"])
        (showOption lineComment showString)
        (showOption blockComment
           (fun (a,b)  ->
              (("(")[@reason.raw_literal "("]) ^
                ((showString a) ^
                   (((", ")[@reason.raw_literal ", "]) ^
                      ((showString b) ^ ((")")[@reason.raw_literal ")"]))))))
        (showList rules
           (fun (name,rule)  ->
              (("(")[@reason.raw_literal "("]) ^
                ((showString name) ^
                   (((", ")[@reason.raw_literal ", "]) ^
                      ((showRule rule) ^ ((")")[@reason.raw_literal ")"]))))))
  end
module DSL =
  struct
    open Parsing
    let star x = ((Star (x)))
    let plus x = ((Plus (x)))
    let maybe x = ((Optional (x)))
    let group x = ((Group (x)))
    let t ?label  t = ((Terminal (t, label)))
    let n ?label  t = ((NonTerminal (t, label)))
    let chars a b = ((Chars (a, b, None)))
    let hugLeft x = ((NoSpaceBefore (x)))
    let hugRight x = ((NoSpaceAfter (x)))
  end
let unwrapOr a b =
  match a with | ((Some (x))) -> x | None  -> b
module Path =
  struct
    type pathItem =
      | Item of Parsing.parsing* int
      | Iter of int
      | Choice of int* string[@@deriving show]
  end
module Error =
  struct
    type errors = (int* (bool* Path.pathItem list) list)[@@deriving show]
    type partial = (int* errors)[@@deriving show]
    let errorText (isNot,rule) =
      match rule with
      | ((Parsing.Terminal (text,label))) ->
          (("Expected \"")[@reason.raw_literal "Expected \\\""]) ^
            ((String.escaped text) ^ (("\"")[@reason.raw_literal "\\\""]))
      | ((Parsing.Chars (start,cend,label))) ->
          Printf.sprintf
            (("Expected %c..%c")[@reason.raw_literal "Expected %c..%c"])
            start cend
      | ((Parsing.NonTerminal (name,label))) -> name
      | ((Parsing.Any (label))) ->
          (("Any")[@reason.raw_literal "Any"])
      | Parsing.Star _ -> (("Star")[@reason.raw_literal "Star"])
      | Parsing.Plus _ -> (("Plus")[@reason.raw_literal "Plus"])
      | Parsing.Optional _ -> (("Optional")[@reason.raw_literal "Optional"])
      | Parsing.EOF  ->
          (("End of Input")[@reason.raw_literal "End of Input"])
      | Parsing.CommentEOL  ->
          (("Expected a newline (with optional comments)")[@reason.raw_literal
                                                            "Expected a newline (with optional comments)"])
      | _ -> (("Unknown problem")[@reason.raw_literal "Unknown problem"])
    let errorPathItemText isNot pathItem =
      match pathItem with
      | ((Path.Choice (n,name))) ->
          (string_of_int n) ^ (((":")[@reason.raw_literal ":"]) ^ name)
      | ((Path.Item (item,loopIndex))) ->
          (string_of_int loopIndex) ^
            (((":")[@reason.raw_literal ":"]) ^ (errorText (isNot, item)))
      | ((Path.Iter (n))) ->
          (("*")[@reason.raw_literal "*"]) ^ (string_of_int n)
    let rec errorPathText isNot path collect =
      match path with
      | [] -> collect
      | pathItem::path ->
          errorPathText isNot path ((errorPathItemText isNot pathItem) ::
            collect)
    let lastLineLength txt pos =
      if pos >= (String.length txt)
      then String.length txt
      else
        (try
           let atNewline = (txt.[pos]) = '\n' in
           let mpos = match atNewline with | true  -> pos - 1 | false  -> pos in
           let lastPos = String.rindex_from txt mpos '\n' in
           (pos - lastPos) - 1
         with | Not_found  -> pos)
    let leftPad base num coll =
      let res = ref "" in
      for i = 0 to num do res := (base ^ (!res)) done; !res
    let rec slice lst start =
      match (start, lst) with
      | (0,_) -> lst
      | (_,_::rest) -> slice rest (start - 1)
      | _ ->
          raise
            ((Failure
                ((("Invalid slice")[@reason.raw_literal "Invalid slice"])))
            )
    let errorsText errors =
      String.concat ""
        (List.map
           (fun (isNot,errPath)  ->
              let parts = errorPathText isNot errPath [] in
              let count = List.length parts in
              let parts =
                if count < 5
                then parts
                else (("...")[@reason.raw_literal "..."]) ::
                  (slice parts (count - 5)) in
              Printf.sprintf (("%s\n")[@reason.raw_literal "%s\\n"])
                (String.concat ((" > ")[@reason.raw_literal " > "]) parts))
           errors)
    let genErrorText text (pos,errors) =
      let (showText,pad) =
        if pos.Lexing.pos_cnum <= 0
        then (text, 0)
        else
          ((String.sub text 0
              (try String.index_from text pos.pos_cnum '\n'
               with | Not_found  -> String.length text)),
            ((lastLineLength text pos.pos_cnum) - 1)) in
      (Printf.sprintf (("%s\n%s^\n")[@reason.raw_literal "%s\\n%s^\\n"])
         showText (leftPad (("-")[@reason.raw_literal "-"]) pad ""))
        ^ (errorsText errors)
  end
exception ConversionError of Location.t* string* string
module Result =
  struct
    type loc = Location.t
    type comment = (string* loc)
    type comments =
      (comment option* comment list* comment list* comment option)
    type rule = (string* string)
    type commentType =
      | Doc
      | Multi
      | EOL
    type result =
      | Leaf of rule* string* loc
      | Comment of commentType* string* loc
      | Node of rule* (string* result) list* loc* comments option
    let loc res =
      match res with
      | ((Leaf (_,_,loc)))|((Comment
        (_,_,loc)))|((Node (_,_,loc,_)))
          -> loc
    let white n =
      let buffer = Buffer.create n in
      for _ = 0 to n do Buffer.add_char buffer ' ' done;
      Buffer.contents buffer
    let showPos { Lexing.pos_lnum = pos_lnum; pos_cnum; pos_bol } =
      Printf.sprintf (("%d:%d(%d)")[@reason.raw_literal "%d:%d(%d)"])
        pos_lnum (pos_cnum - pos_bol) pos_cnum
    let showLoc { Location.loc_start = loc_start; loc_end } =
      (showPos loc_start) ^
        (((" - ")[@reason.raw_literal " - "]) ^ (showPos loc_end))
    let rec showNode label node indent =
      match node with
      | ((Leaf ((rule,sub),string,loc))) ->
          (match label = "" with
           | true  -> ""
           | false  ->
               (("[")[@reason.raw_literal "["]) ^
                 (label ^ (("]")[@reason.raw_literal "]"])))
            ^
            (rule ^
               ((("(")[@reason.raw_literal "("]) ^
                  ((match sub = "" with
                    | true  -> ""
                    | false  -> sub ^ ((", ")[@reason.raw_literal ", "])) ^
                     ((showLoc loc) ^
                        (((")")[@reason.raw_literal ")"]) ^
                           (((": ")[@reason.raw_literal ": "]) ^
                              (String.escaped string)))))))
      | ((Comment (t,string,loc))) ->
          (("Comment: ")[@reason.raw_literal "Comment: "]) ^ string
      | ((Node ((rule,sub),children,loc,comments))) ->
          (match label = "" with
           | true  -> ""
           | false  ->
               (("[")[@reason.raw_literal "["]) ^
                 (label ^ (("]")[@reason.raw_literal "]"])))
            ^
            (rule ^
               ((("(")[@reason.raw_literal "("]) ^
                  ((match sub = "" with
                    | true  -> ""
                    | false  -> sub ^ ((", ")[@reason.raw_literal ", "])) ^
                     ((showLoc loc) ^
                        (((")")[@reason.raw_literal ")"]) ^
                           (String.concat (("\n")[@reason.raw_literal "\\n"])
                              (List.map
                                 (fun (label,node)  ->
                                    (white indent) ^
                                      (showNode label node (indent + 2)))
                                 children)))))))
    type parserMatch = (result,(result option* Error.partial)) Belt.Result.t
  end
end
module ExampleGenerator
= struct
#1 "exampleGenerator.ml"
[@@@ocaml.ppx.context { cookies = [] }]
module P = PackTypes.Parsing
module R = PackTypes.Result
let memo: (string,string) Hashtbl.t = Hashtbl.create 100
let mLoc = Location.none
let optOr orr opt =
  match opt with | None  -> orr | ((Some (x))) -> x
let maybePrint grammar result = None
let rec generateForItem grammar table depth item =
  match item with
  | ((P.NonTerminal (name,label))) ->
      [((optOr "" label), (generateForRule grammar table name (depth + 1)))]
  | ((P.Terminal (contents,label))) ->
      (match label with
       | ((Some (label))) ->
           [(label, ((R.Leaf (("", ""), contents, mLoc))))]
       | None  -> [])
  | ((P.Lexify (p)))|((P.NoSpaceAfter
    (p)))|((P.NoSpaceBefore (p)))
    |((P.NoBreakAfter (p)))|((P.NoBreakBefore
    (p)))|((P.Star (p)))|((P.Plus
    (p)))|((P.Optional (p))) ->
      generateForItem grammar table depth p
  | ((P.Group (p))) ->
      List.concat (List.map (generateForItem grammar table depth) p)
  | P.Not _|P.Any _|P.Lookahead _|P.EOF |P.Empty |P.Indent |P.FullIndent 
    |P.CommentEOL  -> []
  | ((P.Chars (start,cend,label))) ->
      let s = Char.code start in
      [((optOr "" label),
         ((R.Leaf
             (("", ""),
               (Printf.sprintf (("%c")[@reason.raw_literal "%c"])
                  (Char.chr ((Random.int ((Char.code cend) - s)) + s))),
               mLoc))))]
and generateForRule grammar table rulename depth =
  try
    ((R.Leaf ((rulename, ""), (Hashtbl.find table rulename), mLoc)))
  with
  | Not_found  ->
      if depth > 3
      then
        ((R.Leaf
            ((rulename, ""),
              ((("<")[@reason.raw_literal "<"]) ^
                 (rulename ^ ((">")[@reason.raw_literal ">"]))), mLoc))
        )
      else
        (let rule = List.assoc rulename grammar.P.rules in
         let choice = Random.int (List.length rule.P.choices) in
         let (sub,_,items) = List.nth rule.P.choices choice in
         ((R.Node
             ((rulename, sub),
               (List.concat
                  (List.map (generateForItem grammar table depth) items)),
               mLoc, None))))
let generateForChoice grammar table rule items =
  ((R.Node
      (rule,
        (List.concat (List.map (generateForItem grammar table 5) items)),
        mLoc, None)))
let generateExamples grammar ruleName table =
  let { P.choices = choices;_} = List.assoc ruleName grammar.P.rules in
  (List.map
     (fun (sub,comment,items)  ->
        sub ^
          (((":\n")[@reason.raw_literal ":\\n"]) ^
             (((generateForChoice grammar table (ruleName, sub) items) |>
                 (maybePrint grammar))
                |>
                (optOr
                   (("Got nothing while printing")[@reason.raw_literal
                                                    "Got nothing while printing"])))))
     choices)
    |> (String.concat (("\n")[@reason.raw_literal "\\n"]))
let isLexical ruleName =
  let l = String.sub ruleName 0 1 in l <> (String.capitalize l)
let rec simpleForItem grammar item =
  match item with
  | ((P.NonTerminal (name,label))) ->
      simpleForRule grammar name
  | ((P.Terminal (contents,label))) ->
      [`Text
         ((let c =
             ((contents |>
                 (Str.global_replace
                    (Str.regexp_string (("|")[@reason.raw_literal "|"]))
                    (("\\|")[@reason.raw_literal "\\\\|"])))
                |>
                (Str.global_replace
                   (Str.regexp_string (("<")[@reason.raw_literal "<"]))
                   (("&lt;")[@reason.raw_literal "&lt;"])))
               |>
               (Str.global_replace
                  (Str.regexp_string ((">")[@reason.raw_literal ">"]))
                  (("&gt;")[@reason.raw_literal "&gt;"])) in
           if c = (("\\")[@reason.raw_literal "\\\\"])
           then (("\\\\")[@reason.raw_literal "\\\\\\\\"])
           else c))]
  | ((P.NoBreakAfter (p)))|((P.NoBreakBefore
    (p))) -> simpleForItem grammar p
  | ((P.NoSpaceAfter (p))) ->
      (simpleForItem grammar p) @ [`Collapse]
  | ((P.NoSpaceBefore (p))) -> `Collapse ::
      (simpleForItem grammar p)
  | ((P.Lexify (p)))|((P.Star (p))) ->
      (simpleForItem grammar p) @
        [`Collapse;
        `Text (("<sup>*</sup>")[@reason.raw_literal "<sup>*</sup>"])]
  | ((P.Plus (p))) ->
      (simpleForItem grammar p) @
        [`Collapse;
        `Text (("<sup>+</sup>")[@reason.raw_literal "<sup>+</sup>"])]
  | ((P.Optional (p))) ->
      (simpleForItem grammar p) @
        [`Collapse;
        `Text (("<sup>?</sup>")[@reason.raw_literal "<sup>?</sup>"])]
  | ((P.Group (p))) ->
      ((`Text (("\226\166\133")[@reason.raw_literal "\226\166\133"])) ::
        `Collapse :: (List.concat (List.map (simpleForItem grammar) p))) @
        [`Collapse;
        `Text (("\226\166\134")[@reason.raw_literal "\226\166\134"])]
  | P.Any _ -> [`Text (("<i>any</i>")[@reason.raw_literal "<i>any</i>"])]
  | P.Not _|P.Lookahead _|P.EOF |P.Empty |P.Indent |P.FullIndent 
    |P.CommentEOL  -> []
  | ((P.Chars (start,cend,label))) ->
      [`Text
         ((Char.escaped start) ^
            ((("\226\128\166")[@reason.raw_literal "\226\128\166"]) ^
               (Char.escaped cend)))]
and simpleForRule grammar rulename =
  let rule = List.assoc rulename grammar.P.rules in
  let (sub,comment,items) = List.hd rule.P.choices in
  if ((List.length rule.P.choices) > 1) || ((comment <> "") || rule.leaf)
  then
    [`Text
       ((("<a href=\"#")[@reason.raw_literal "<a href=\\\"#"]) ^
          ((String.lowercase rulename) ^
             ((("\">")[@reason.raw_literal "\\\">"]) ^
                (rulename ^ (("</a>")[@reason.raw_literal "</a>"])))))]
  else
    (let res = List.concat (List.map (simpleForItem grammar) items) in
     if isLexical rulename
     then
       [`Text
          (String.concat ""
             (List.map
                (fun m  -> match m with | `Text a -> a | `Collapse -> "") res))]
     else res)
let help =
  {|
This grammar is displayed using a syntax similar to regular expressions.

- `⦅` and `⦆` "thick parenthesis" indicate grouping, to distinguish from parenthesis that are actually part of the grammar
- <code><sup>+</sup></code> allows one or more of the preceeding term or group
- <code><sup>*</sup></code> allows zero or more of the preceeding term or group
- <code><sup>?</sup></code> allows zero or one of the preceeding term or group

Note that this sacrifices a bit of precision in the interest of readability. For the source of truth, read the source code :D
|}
let simpleForChoice grammar items =
  List.concat (List.map (simpleForItem grammar) items)
let rec showSimple items =
  match items with
  | (`Text a)::(`Text b)::rest ->
      a ^
        (((" ")[@reason.raw_literal " "]) ^ (showSimple ((`Text b) :: rest)))
  | (`Text a)::`Collapse::rest -> a ^ (showSimple rest)
  | (`Text a)::[] -> a
  | `Collapse::rest -> showSimple rest
  | [] -> ""
let showSimple items ruleName =
  let isLexical =
    let l = String.sub ruleName 0 1 in l <> (String.capitalize l) in
  if isLexical
  then
    String.concat ""
      (List.map (fun m  -> match m with | `Text a -> a | `Collapse -> "")
         items)
  else showSimple items
let docsForGrammar grammar =
  let open PackTypes.Parsing in
    (List.map
       (fun (name,rule)  ->
          let (sub,comment,items) = List.hd rule.choices in
          if (List.length rule.choices) > 1
          then
            (Printf.sprintf
               (("### %s\n\n")[@reason.raw_literal "### %s\\n\\n"]) name)
              ^
              ((match rule.docs with
                | None  -> ""
                | ((Some (docs))) ->
                    docs ^ (("\n\n")[@reason.raw_literal "\\n\\n"]))
                 ^
                 ((("| Name | Syntax |\n| --- | --- |\n")[@reason.raw_literal
                                                           "| Name | Syntax |\\n| --- | --- |\\n"])
                    ^
                    (((rule.choices |>
                         (List.map
                            (fun (sub,comment,items)  ->
                               (("| <i>")[@reason.raw_literal "| <i>"]) ^
                                 (sub ^
                                    ((("</i> | ")[@reason.raw_literal
                                                   "</i> | "])
                                       ^
                                       ((("<code>")[@reason.raw_literal
                                                     "<code>"])
                                          ^
                                          ((showSimple
                                              (simpleForChoice grammar items)
                                              name)
                                             ^
                                             (("</code> |")[@reason.raw_literal
                                                             "</code> |"]))))))))
                        |>
                        (String.concat (("\n")[@reason.raw_literal "\\n"])))
                       ^ (("\n\n")[@reason.raw_literal "\\n\\n"]))))
          else
            if comment <> ""
            then
              (("### ")[@reason.raw_literal "### "]) ^
                (name ^
                   ((("\n\n")[@reason.raw_literal "\\n\\n"]) ^
                      (comment ^
                         ((("\n\n")[@reason.raw_literal "\\n\\n"]) ^
                            ((("<code>")[@reason.raw_literal "<code>"]) ^
                               ((showSimple (simpleForChoice grammar items)
                                   name)
                                  ^
                                  (("</code>\n\n")[@reason.raw_literal
                                                    "</code>\\n\\n"])))))))
            else "") (grammar.rules |> List.rev))
      |> (String.concat "")
end
module GrammarGrammar
= struct
#1 "grammarGrammar.ml"
[@@@ocaml.ppx.context { cookies = [] }]
open PackTypes.Parsing
let grammar =
  {
    lineComment =
      ((Some (((";")[@reason.raw_literal ";"]))));
    blockComment =
      ((Some
          (((("/*")[@reason.raw_literal "/*"]),
             (("*/")[@reason.raw_literal "*/"])))));
    rules =
      [((("Start")[@reason.raw_literal "Start"]),
         {
           passThrough = false;
           ignoreNewlines = Inherit;
           capturesComments = false;
           preserveInnerSpace = false;
           leaf = false;
           docs = None;
           choices =
             [("", "",
                [((Star
                     (((NonTerminal
                          ((("Rule")[@reason.raw_literal "Rule"]), None))
                       ))))])]
         });
      ((("Decorator")[@reason.raw_literal "Decorator"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("@")[@reason.raw_literal "@"]), None)));
               ((NonTerminal
                   ((("ident")[@reason.raw_literal "ident"]),
                     ((Some ((("name")[@reason.raw_literal "name"])))
                     ))));
               ((Optional
                   (((Group
                        ([((Terminal ((("(")[@reason.raw_literal "("]), None))
                         );
                         ((Star
                             (((Group
                                  ([((NonTerminal
                                        ((("decarg")[@reason.raw_literal
                                                      "decarg"]),
                                          ((Some
                                              ((("args")[@reason.raw_literal
                                                          "args"]))))))
                                   );
                                   ((Terminal
                                       (((",")[@reason.raw_literal ","]),
                                         None)))]))
                               ))));
                         ((Optional
                             (((NonTerminal
                                  ((("decarg")[@reason.raw_literal "decarg"]),
                                    ((Some
                                        ((("args")[@reason.raw_literal
                                                    "args"])))))))))
                         );
                         ((Terminal (((")")[@reason.raw_literal ")"]), None))
                         )]))))));
               CommentEOL])]
        });
      ((("decarg")[@reason.raw_literal "decarg"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [((("bool")[@reason.raw_literal "bool"]), "",
               [((NonTerminal ((("bool")[@reason.raw_literal "bool"]), None))
               )]);
            ((("string")[@reason.raw_literal "string"]), "",
              [((NonTerminal
                   ((("string")[@reason.raw_literal "string"]), None))
              )]);
            ((("number")[@reason.raw_literal "number"]), "",
              [((NonTerminal
                   ((("number")[@reason.raw_literal "number"]), None))
              )])]
        });
      ((("bool")[@reason.raw_literal "bool"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = true;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("true")[@reason.raw_literal "true"]), None))
               )]);
            ("", "",
              [((Terminal ((("false")[@reason.raw_literal "false"]), None))
              )])]
        });
      ((("Rule")[@reason.raw_literal "Rule"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Star
                    (((NonTerminal
                         ((("Decorator")[@reason.raw_literal "Decorator"]),
                           ((Some
                               ((("decorators")[@reason.raw_literal
                                                 "decorators"]))))))
                      ))));
               ((NonTerminal
                   ((("ident")[@reason.raw_literal "ident"]),
                     ((Some ((("name")[@reason.raw_literal "name"])))
                     ))));
               ((Terminal ((("=")[@reason.raw_literal "="]), None)));
               ((NonTerminal
                   ((("Choice")[@reason.raw_literal "Choice"]),
                     ((Some ((("choices")[@reason.raw_literal "choices"])))
                     ))));
               CommentEOL]);
            ("", "",
              [((Star
                   (((NonTerminal
                        ((("Decorator")[@reason.raw_literal "Decorator"]),
                          ((Some
                              ((("decorators")[@reason.raw_literal
                                                "decorators"]))))))
                     ))));
              ((NonTerminal
                  ((("ident")[@reason.raw_literal "ident"]),
                    ((Some ((("name")[@reason.raw_literal "name"]))))))
              );
              ((Terminal ((("=")[@reason.raw_literal "="]), None)));
              CommentEOL;
              ((Plus
                  (((Group
                       ([((Terminal ((("|")[@reason.raw_literal "|"]), None))
                        );
                        ((NonTerminal
                            ((("Choice")[@reason.raw_literal "Choice"]),
                              ((Some
                                  ((("choices")[@reason.raw_literal
                                                 "choices"])))))));
                        CommentEOL]))))))])]
        });
      ((("Choice")[@reason.raw_literal "Choice"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Plus
                    (((NonTerminal
                         ((("Item")[@reason.raw_literal "Item"]), None))
                      ))));
               ((Optional
                   (((Group
                        ([((Terminal
                              ((("--")[@reason.raw_literal "--"]), None))
                         );
                         ((NonTerminal
                             ((("ident")[@reason.raw_literal "ident"]),
                               ((Some
                                   ((("name")[@reason.raw_literal "name"])))
                               ))))]))
                     ))));
               ((Optional
                   (((Group
                        ([((Terminal (((";")[@reason.raw_literal ";"]), None))
                         );
                         ((NonTerminal
                             ((("rest_of_line")[@reason.raw_literal
                                                 "rest_of_line"]),
                               ((Some
                                   ((("comment")[@reason.raw_literal
                                                  "comment"]))))))
                         )]))))))])]
        });
      ((("Item")[@reason.raw_literal "Item"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [((("full_indent")[@reason.raw_literal "full_indent"]), "",
               [((Terminal (((">>")[@reason.raw_literal ">>"]), None))
               )]);
            ((("indent")[@reason.raw_literal "indent"]), "",
              [((Terminal (((">")[@reason.raw_literal ">"]), None)))]);
            ((("item")[@reason.raw_literal "item"]), "",
              [((NonTerminal ((("Item_")[@reason.raw_literal "Item_"]), None))
              )])]
        });
      ((("Item_")[@reason.raw_literal "Item_"]),
        {
          passThrough = true;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Optional
                    (((NoSpaceAfter
                         (((Terminal
                              ((("~")[@reason.raw_literal "~"]),
                                ((Some ((("neg")[@reason.raw_literal "neg"])))
                                ))))))
                      ))));
               ((Optional
                   (((NoSpaceAfter
                        (((Terminal
                             ((("#")[@reason.raw_literal "#"]),
                               ((Some
                                   ((("lexify")[@reason.raw_literal "lexify"])))
                               ))))))
                     ))));
               ((Optional
                   (((NoSpaceAfter
                        (((Group
                             ([((NoSpaceAfter
                                   (((Terminal
                                        ((("[")[@reason.raw_literal "["]),
                                          None)))))
                              );
                              ((Optional
                                  (((NoSpaceAfter
                                       (((NonTerminal
                                            ((("flag")[@reason.raw_literal
                                                        "flag"]),
                                              ((Some
                                                  ((("flag")[@reason.raw_literal
                                                              "flag"])))
                                              ))))))
                                    ))));
                              ((NoSpaceAfter
                                  (((NonTerminal
                                       ((("ident")[@reason.raw_literal
                                                    "ident"]),
                                         ((Some
                                             ((("name")[@reason.raw_literal
                                                         "name"]))))))
                                    ))));
                              ((Terminal
                                  ((("]")[@reason.raw_literal "]"]), None))
                              )])))))
                     ))));
               ((Optional
                   (((NoSpaceBefore
                        (((NonTerminal
                             ((("noBreak")[@reason.raw_literal "noBreak"]),
                               ((Some
                                   ((("noBreakBefore")[@reason.raw_literal
                                                        "noBreakBefore"])))
                               ))))))
                     ))));
               ((Optional
                   (((NoSpaceAfter
                        (((NonTerminal
                             ((("noSpace")[@reason.raw_literal "noSpace"]),
                               ((Some
                                   ((("noSpaceBefore")[@reason.raw_literal
                                                        "noSpaceBefore"])))
                               ))))))
                     ))));
               ((NonTerminal
                   ((("ItemInner")[@reason.raw_literal "ItemInner"]),
                     ((Some ((("inner")[@reason.raw_literal "inner"])))
                     ))));
               ((Optional
                   (((NoSpaceBefore
                        (((NonTerminal
                             ((("noSpace")[@reason.raw_literal "noSpace"]),
                               ((Some
                                   ((("noSpaceAfter")[@reason.raw_literal
                                                       "noSpaceAfter"])))
                               ))))))
                     ))));
               ((Optional
                   (((NoSpaceBefore
                        (((NonTerminal
                             ((("noBreak")[@reason.raw_literal "noBreak"]),
                               ((Some
                                   ((("noBreakAfter")[@reason.raw_literal
                                                       "noBreakAfter"])))
                               ))))))
                     ))));
               ((Optional
                   (((NoSpaceBefore
                        (((NonTerminal
                             ((("suffix")[@reason.raw_literal "suffix"]),
                               ((Some
                                   ((("suffix")[@reason.raw_literal "suffix"])))
                               ))))))
                     ))))])]
        });
      ((("noSpace")[@reason.raw_literal "noSpace"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = true;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("&")[@reason.raw_literal "&"]), None)))])]
        });
      ((("noBreak")[@reason.raw_literal "noBreak"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = true;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("$")[@reason.raw_literal "$"]), None)))])]
        });
      ((("ItemInner")[@reason.raw_literal "ItemInner"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((NonTerminal
                    ((("string")[@reason.raw_literal "string"]), None))
               )]);
            ("", "",
              [((NonTerminal ((("ident")[@reason.raw_literal "ident"]), None))
              )]);
            ((("nested")[@reason.raw_literal "nested"]), "",
              [((Terminal ((("(")[@reason.raw_literal "("]), None)));
              ((NonTerminal
                  ((("NestedItems")[@reason.raw_literal "NestedItems"]),
                    None)));
              ((Terminal (((")")[@reason.raw_literal ")"]), None)))]);
            ("", "",
              [((NonTerminal
                   ((("char_range")[@reason.raw_literal "char_range"]), None))
              )]);
            ("", "",
              [((NonTerminal ((("char")[@reason.raw_literal "char"]), None))
              )])]
        });
      ((("NestedItems")[@reason.raw_literal "NestedItems"]),
        {
          passThrough = true;
          ignoreNewlines = Yes;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Plus
                    (((NonTerminal
                         ((("Item")[@reason.raw_literal "Item"]),
                           ((Some
                               ((("nested")[@reason.raw_literal "nested"])))
                           ))))))
               )])]
        });
      ((("char_range")[@reason.raw_literal "char_range"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("'")[@reason.raw_literal "'"]), None)));
               ((NonTerminal
                   ((("single")[@reason.raw_literal "single"]),
                     ((Some ((("start")[@reason.raw_literal "start"])))
                     ))));
               ((Terminal ((("..")[@reason.raw_literal ".."]), None))
               );
               ((NonTerminal
                   ((("single")[@reason.raw_literal "single"]),
                     ((Some ((("end")[@reason.raw_literal "end"]))))))
               );
               ((Terminal ((("'")[@reason.raw_literal "'"]), None)))])]
        });
      ((("char")[@reason.raw_literal "char"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("'")[@reason.raw_literal "'"]), None)));
               ((NonTerminal
                   ((("single")[@reason.raw_literal "single"]),
                     ((Some ((("char")[@reason.raw_literal "char"])))
                     ))));
               ((Terminal ((("'")[@reason.raw_literal "'"]), None)))])]
        });
      ((("single")[@reason.raw_literal "single"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = true;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("\\")[@reason.raw_literal "\\\\"]), None))
               );
               ((Any (None)))]);
            ("", "",
              [((Not
                   (((Terminal ((("'")[@reason.raw_literal "'"]), None))
                     ))));
              ((Not
                  (((Terminal ((("\n")[@reason.raw_literal "\\n"]), None))
                    ))));
              ((Any (None)))])]
        });
      ((("string")[@reason.raw_literal "string"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = true;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("\"")[@reason.raw_literal "\\\""]), None))
               );
               ((Star
                   (((NonTerminal
                        ((("strchar")[@reason.raw_literal "strchar"]),
                          ((Some
                              ((("contents")[@reason.raw_literal "contents"])))
                          )))))));
               ((Terminal ((("\"")[@reason.raw_literal "\\\""]), None))
               )])]
        });
      ((("strchar")[@reason.raw_literal "strchar"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("\\")[@reason.raw_literal "\\\\"]), None))
               );
               ((Any (None)))]);
            ("", "",
              [((Not
                   (((Terminal ((("\"")[@reason.raw_literal "\\\""]), None))
                     ))));
              ((Not
                  (((Terminal ((("\n")[@reason.raw_literal "\\n"]), None))
                    ))));
              ((Any (None)))])]
        });
      ((("flag")[@reason.raw_literal "flag"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [((("bool")[@reason.raw_literal "bool"]),
               (("exists")[@reason.raw_literal "exists"]),
               [((Terminal ((("?")[@reason.raw_literal "?"]), None)))]);
            ((("array")[@reason.raw_literal "array"]), "",
              [((Terminal (((":")[@reason.raw_literal ":"]), None)))]);
            ((("string")[@reason.raw_literal "string"]),
              (("contents")[@reason.raw_literal "contents"]),
              [((Terminal ((("@")[@reason.raw_literal "@"]), None)))])]
        });
      ((("suffix")[@reason.raw_literal "suffix"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [((("plus")[@reason.raw_literal "plus"]), "",
               [((Terminal ((("+")[@reason.raw_literal "+"]), None)))]);
            ((("star")[@reason.raw_literal "star"]), "",
              [((Terminal ((("*")[@reason.raw_literal "*"]), None)))]);
            ((("opt")[@reason.raw_literal "opt"]), "",
              [((Terminal ((("?")[@reason.raw_literal "?"]), None)))])]
        });
      ((("ident")[@reason.raw_literal "ident"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = true;
          docs = None;
          choices =
            [("", "",
               [((Not
                    (((NonTerminal
                         ((("digit")[@reason.raw_literal "digit"]), None))
                      ))));
               ((Plus
                   (((NonTerminal
                        ((("identchar")[@reason.raw_literal "identchar"]),
                          None))))))])]
        });
      ((("identchar")[@reason.raw_literal "identchar"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "", [((Chars ('a', 'z', None)))]);
            ("", "", [((Chars ('A', 'Z', None)))]);
            ("", "", [((Chars ('0', '9', None)))]);
            ("", "",
              [((Terminal ((("_")[@reason.raw_literal "_"]), None)))])]
        });
      ((("number")[@reason.raw_literal "number"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = true;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("0")[@reason.raw_literal "0"]), None)));
               ((Not
                   (((NonTerminal
                        ((("identchar")[@reason.raw_literal "identchar"]),
                          None))))))]);
            ("", "",
              [((Not
                   (((Terminal ((("0")[@reason.raw_literal "0"]), None))
                     ))));
              ((Plus
                  (((NonTerminal
                       ((("digit")[@reason.raw_literal "digit"]), None))
                    ))));
              ((Not
                  (((NonTerminal
                       ((("identchar")[@reason.raw_literal "identchar"]),
                         None))))))])]
        });
      ((("digit")[@reason.raw_literal "digit"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "", [((Chars ('0', '9', None)))])]
        });
      ((("rest_of_line")[@reason.raw_literal "rest_of_line"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = true;
          docs = None;
          choices =
            [("", "",
               [((Star
                    (((Group
                         ([((Not
                               (((Terminal
                                    ((("\n")[@reason.raw_literal "\\n"]),
                                      None))))));
                          ((Any (None)))]))))))])]
        });
      ((("eol")[@reason.raw_literal "eol"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Star
                    (((NonTerminal
                         ((("white")[@reason.raw_literal "white"]), None))
                      ))));
               ((NonTerminal ((("eee")[@reason.raw_literal "eee"]), None))
               )])]
        });
      ((("eee")[@reason.raw_literal "eee"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Plus
                    (((NonTerminal
                         ((("eolchar")[@reason.raw_literal "eolchar"]), None))
                      ))))]);
            ("", "", [EOF])]
        });
      ((("eolchar")[@reason.raw_literal "eolchar"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Terminal ((("\n")[@reason.raw_literal "\\n"]), None))
               )]);
            ("", "",
              [((Terminal ((("\r")[@reason.raw_literal "\\r"]), None))
              )])]
        });
      ((("white")[@reason.raw_literal "white"]),
        {
          passThrough = false;
          ignoreNewlines = Inherit;
          capturesComments = false;
          preserveInnerSpace = false;
          leaf = false;
          docs = None;
          choices =
            [("", "",
               [((Terminal (((" ")[@reason.raw_literal " "]), None)))]);
            ("", "",
              [((Terminal ((("\t")[@reason.raw_literal "\\t"]), None))
              )])]
        })]
  }
end
module ResultUtils
= struct
#1 "resultUtils.ml"
[@@@ocaml.ppx.context { cookies = [] }]
open PackTypes.Result
module P = PackTypes.Parsing
let rec getChild children mapper =
  match children with
  | [] -> None
  | child::rest ->
      (match mapper child with | None  -> getChild rest mapper | x -> x)
let rec getChildren children mapper =
  match children with
  | [] -> []
  | child::rest ->
      (match mapper child with
       | None  -> getChildren rest mapper
       | ((Some (x))) -> x :: (getChildren rest mapper))
let getContentsByLabel children needle =
  getChild children
    (fun (label,child)  ->
       if needle <> label
       then None
       else
         (match child with
          | ((Leaf (_,contents,_))) ->
              ((Some (contents)))
          | Comment _|Node _ ->
              failwith
                (("expected a leaf")[@reason.raw_literal "expected a leaf"])))
let getManyContentsByType children needle =
  getChildren children
    (fun child  ->
       match child with
       | (_,((Leaf ((name,_),contents,_)))) when
           name = needle -> ((Some (contents)))
       | (_,((Node ((name,_),_,_,_)))) when name = needle
           ->
           failwith
             (("expected a leaf")[@reason.raw_literal "expected a leaf"])
       | _ -> None)
let getContentsByType children needle =
  getChild children
    (fun child  ->
       match child with
       | (_,((Leaf ((name,_),contents,_)))) when
           name = needle -> ((Some (contents)))
       | (_,((Node ((name,_),_,_,_)))) when name = needle
           ->
           failwith
             (("expected a leaf")[@reason.raw_literal "expected a leaf"])
       | _ -> None)
let rec getPresence children mapper =
  match children with
  | [] -> false
  | child::rest ->
      (match mapper child with | false  -> getPresence rest mapper | x -> x)
let getPresenceByLabel children needle =
  getPresence children
    (fun child  ->
       match child with | (name,_) when name = needle -> true | _ -> false)
let getPresenceByType children needle =
  getPresence children
    (fun child  ->
       match child with
       | (_,((Leaf ((name,_),_,_))))
         |(_,((Node ((name,_),_,_,_)))) when name = needle
           -> true
       | _ -> false)
let getNodeByType children needle =
  getChild children
    (fun (label,child)  ->
       if label <> ""
       then None
       else
         (match child with
          | ((Node ((name,sub),children,loc,comments)))
              when name = needle ->
              ((Some ((sub, children, loc, comments))))
          | _ -> None))
let getNodesByType children needle nodeMapper =
  getChildren children
    (fun (label,child)  ->
       match child with
       | ((Node ((name,sub),children,loc,comments))) when
           name = needle ->
           ((Some ((nodeMapper (sub, children, loc, comments)))))
       | _ -> None)
let getNodesByLabel children needle nodeMapper =
  getChildren children
    (fun (label,child)  ->
       if label = needle
       then
         match child with
         | ((Node ((name,sub),children,loc,comments))) ->
             ((Some ((nodeMapper (sub, children, loc, comments)))))
         | _ -> None
       else None)
let getNodeByLabel children needle =
  getChild children
    (fun (label,child)  ->
       if label = needle
       then
         match child with
         | ((Node (rule,children,loc,comments))) ->
             ((Some ((rule, children, loc, comments))))
         | Comment _|Leaf _ ->
             failwith
               ((("Expected node for label ")[@reason.raw_literal
                                               "Expected node for label "])
                  ^ needle)
       else None)
let getLeafsByType children needle =
  getChildren children
    (fun (label,child)  ->
       match child with
       | ((Leaf ((name,sub),contents,loc))) when
           name = needle -> ((Some ((contents, loc))))
       | _ -> None)
let getLeafByType children needle =
  getChild children
    (fun (label,child)  ->
       match child with
       | ((Leaf ((name,sub),contents,loc))) when
           name = needle -> ((Some ((contents, loc))))
       | _ -> None)
let getLeafByLabel children needle =
  getChild children
    (fun (label,child)  ->
       match child with
       | ((Leaf ((name,sub),contents,loc))) when
           label = needle -> ((Some ((contents, loc))))
       | _ -> None)
let unescapeString x =
  let contents = String.sub x 1 ((String.length x) - 2) in
  if (String.length contents) = 1 then contents else Scanf.unescaped contents
let unescapeChar x =
  if (String.length x) = 1 then x.[0] else (unescapeString x).[0]
exception ConversionFailure of string
let unwrap opt =
  match opt with
  | ((Some (x))) -> x
  | None  ->
      raise
        ((ConversionFailure
            ((("Unwrapping none")[@reason.raw_literal "Unwrapping none"])))
        )
let assertEq one two =
  if one <> two
  then
    raise
      ((ConversionFailure
          ((("Assertion error")[@reason.raw_literal "Assertion error"])))
      )
end
module GrammarOfGrammar
= struct
#1 "grammarOfGrammar.ml"
[@@@ocaml.ppx.context { cookies = [] }]
open PackTypes.Result
module P = PackTypes.Parsing
module RU = ResultUtils
type decoratorArg =
  | Bool of bool
  | String of string
  | Number of int
let parseString contents =
  (String.sub contents 1 ((String.length contents) - 2)) |> Scanf.unescaped
let parseDecorator children =
  let name =
    (RU.getContentsByLabel children (("name")[@reason.raw_literal "name"]))
      |> RU.unwrap in
  let args =
    RU.getChildren children
      (fun child  ->
         match child with
         | ((("args")[@reason.raw_literal "args"]),((Node
            (((("decarg")[@reason.raw_literal "decarg"]),sub),children,_,_))
            )) ->
             ((Some
                 (((match sub with
                    | (("bool")[@reason.raw_literal "bool"]) ->
                        ((Bool
                            (((match (RU.unwrap
                                        (RU.getContentsByType children
                                           (("bool")[@reason.raw_literal
                                                      "bool"])))
                                       =
                                       (("true")[@reason.raw_literal "true"])
                               with
                               | true  -> true
                               | false  -> false)))))
                    | (("string")[@reason.raw_literal "string"]) ->
                        ((String
                            ((parseString
                                (RU.unwrap
                                   (RU.getContentsByType children
                                      (("string")[@reason.raw_literal
                                                   "string"])))))))
                    | (("number")[@reason.raw_literal "number"]) ->
                        ((Number
                            ((int_of_string
                                (RU.unwrap
                                   (RU.getContentsByType children
                                      (("number")[@reason.raw_literal
                                                   "number"])))))))
                    | _ ->
                        failwith
                          (("unexpected arg type")[@reason.raw_literal
                                                    "unexpected arg type"])))))
             )
         | _ -> None) in
  (name, args)
let optOr orr opt =
  match opt with | ((Some (x))) -> x | None  -> orr
let getFlag children =
  RU.getChild children
    (fun child  ->
       match child with
       | ((("flag")[@reason.raw_literal "flag"]),((Node
          ((_,sub),_,_,_)))) ->
           ((Some (sub)))
       | ((("flag")[@reason.raw_literal "flag"]),_) ->
           failwith
             (("Flag expected to be non-leaf")[@reason.raw_literal
                                                "Flag expected to be non-leaf"])
       | _ -> None)
let unescapeString txt =
  try
    match (String.length txt) = 1 with
    | true  -> txt
    | false  -> Scanf.unescaped txt
  with
  | ((Scanf.Scan_failure (message))) ->
      failwith
        ((("Unescape fail --")[@reason.raw_literal "Unescape fail --"]) ^
           (txt ^ (("--")[@reason.raw_literal "--"])))
let unescapeChar txt =
  (match (String.length txt) = 1 with
   | true  -> txt
   | false  -> unescapeString txt).[0]
let isSome x = match x with | Some _ -> true | None  -> false
let getSuffix children =
  RU.getChild children
    (fun child  ->
       match child with
       | ((("suffix")[@reason.raw_literal "suffix"]),((Node
          ((_,sub),_,_,_)))) ->
           ((Some (sub)))
       | ((("suffix")[@reason.raw_literal "suffix"]),_) ->
           failwith
             (("Suffix expected to be non-leaf")[@reason.raw_literal
                                                  "Suffix expected to be non-leaf"])
       | _ -> None)
let unwrapString txt =
  unescapeString (String.sub txt 1 ((String.length txt) - 2))
let rec parseInner label ((_,sub),children,loc,_) =
  if sub = (("nested")[@reason.raw_literal "nested"])
  then
    (if isSome label
     then
       failwith
         ((("groups can't have labels: ")[@reason.raw_literal
                                           "groups can't have labels: "])
            ^ (RU.unwrap label));
     ((P.Group
         ((RU.getChildren children
             (fun (label,child)  ->
                if label = (("nested")[@reason.raw_literal "nested"])
                then
                  match child with
                  | ((Node
                      (((("Item")[@reason.raw_literal "Item"]),(("item")
                        [@reason.raw_literal "item"])),children,_,_))
                      ) ->
                      ((Some ((parseItem children))))
                  | ((Node
                      (((("Item")[@reason.raw_literal "Item"]),(("indent")
                        [@reason.raw_literal "indent"])),_,_,_)))
                      -> ((Some (P.Indent)))
                  | ((Node
                      (((("Item")[@reason.raw_literal "Item"]),(("full_indent")
                        [@reason.raw_literal "full_indent"])),_,_,_))
                      ) ->
                      ((Some (P.FullIndent)))
                  | _ ->
                      failwith
                        (("Nested child expected to be non-leaf")[@reason.raw_literal
                                                                   "Nested child expected to be non-leaf"])
                else None))))))
  else
    (RU.getChild children
       (fun (_,child)  ->
          match child with
          | ((Leaf
              (((("string")[@reason.raw_literal "string"]),_),contents,_))
              ) ->
              ((Some
                  (((P.Terminal ((unwrapString contents), label)))))
              )
          | ((Leaf
              (((("ident")[@reason.raw_literal "ident"]),_),(("any")[@reason.raw_literal
                                                                    "any"]),_))
              ) ->
              ((Some (((P.Any (label))))))
          | ((Leaf
              (((("ident")[@reason.raw_literal "ident"]),_),(("EOF")[@reason.raw_literal
                                                                    "EOF"]),_))
              ) -> ((Some (P.EOF)))
          | ((Leaf
              (((("ident")[@reason.raw_literal "ident"]),_),(("EOL")[@reason.raw_literal
                                                                    "EOL"]),_))
              ) ->
              ((Some (P.CommentEOL)))
          | ((Leaf
              (((("ident")[@reason.raw_literal "ident"]),_),contents,_))
              ) ->
              ((Some (((P.NonTerminal (contents, label)))))
              )
          | ((Node
              (((("char")[@reason.raw_literal "char"]),_),children,_,_))
              ) ->
              RU.getChild children
                (fun (_,child)  ->
                   match child with
                   | ((Leaf
                       (((("single")[@reason.raw_literal "single"]),_),contents,_))
                       ) ->
                       ((Some
                           (((P.Terminal ((unescapeString contents), label))
                             ))))
                   | _ -> None)
          | ((Node
              (((("char_range")[@reason.raw_literal "char_range"]),_),children,_,_))
              ) ->
              let start =
                (RU.getContentsByLabel children
                   (("start")[@reason.raw_literal "start"]))
                  |> RU.unwrap in
              let send =
                (RU.getContentsByLabel children
                   (("end")[@reason.raw_literal "end"]))
                  |> RU.unwrap in
              ((Some
                  (((P.Chars
                       ((unescapeChar start), (unescapeChar send), label))
                    ))))
          | _ -> None))
      |> RU.unwrap
and parseItem children =
  let neg =
    RU.getPresenceByLabel children (("neg")[@reason.raw_literal "neg"]) in
  let lexify =
    RU.getPresenceByLabel children (("lexify")[@reason.raw_literal "lexify"]) in
  let noBreakAfter =
    RU.getPresenceByLabel children
      (("noBreakAfter")[@reason.raw_literal "noBreakAfter"]) in
  let noBreakBefore =
    RU.getPresenceByLabel children
      (("noBreakBefore")[@reason.raw_literal "noBreakBefore"]) in
  let noSpaceAfter =
    RU.getPresenceByLabel children
      (("noSpaceAfter")[@reason.raw_literal "noSpaceAfter"]) in
  let noSpaceBefore =
    RU.getPresenceByLabel children
      (("noSpaceBefore")[@reason.raw_literal "noSpaceBefore"]) in
  let suffix = getSuffix children in
  let _ = getFlag children in
  let label =
    RU.getContentsByLabel children (("name")[@reason.raw_literal "name"]) in
  let inner =
    ((RU.getNodeByLabel children (("inner")[@reason.raw_literal "inner"])) |>
       RU.unwrap)
      |> (parseInner label) in
  let inner =
    match noBreakAfter with
    | true  -> ((P.NoBreakAfter (inner)))
    | false  -> inner in
  let inner =
    match noBreakBefore with
    | true  -> ((P.NoBreakBefore (inner)))
    | false  -> inner in
  let inner =
    match noSpaceAfter with
    | true  -> ((P.NoSpaceAfter (inner)))
    | false  -> inner in
  let inner =
    match noSpaceBefore with
    | true  -> ((P.NoSpaceBefore (inner)))
    | false  -> inner in
  let inner =
    match suffix with
    | None  -> inner
    | ((Some ((("plus")[@reason.raw_literal "plus"])))) ->
        ((P.Plus (inner)))
    | ((Some ((("star")[@reason.raw_literal "star"])))) ->
        ((P.Star (inner)))
    | ((Some ((("opt")[@reason.raw_literal "opt"])))) ->
        ((P.Optional (inner)))
    | _ ->
        failwith
          (("unexpected suffix")[@reason.raw_literal "unexpected suffix"]) in
  let inner =
    match neg with
    | true  -> ((P.Not (inner)))
    | false  -> inner in
  let inner =
    match lexify with
    | true  -> ((P.Lexify (inner)))
    | false  -> inner in
  inner
let parseChoice children =
  let name =
    (RU.getContentsByLabel children (("name")[@reason.raw_literal "name"]))
      |> (optOr "") in
  let comment =
    (RU.getContentsByLabel children
       (("comment")[@reason.raw_literal "comment"]))
      |> (optOr "") in
  let children =
    RU.getChildren children
      (fun (label,child)  ->
         match child with
         | ((Node
             (((("Item")[@reason.raw_literal "Item"]),(("item")[@reason.raw_literal
                                                                 "item"])),children,_,_))
             ) ->
             ((Some ((parseItem children))))
         | ((Node
             (((("Item")[@reason.raw_literal "Item"]),(("indent")[@reason.raw_literal
                                                                   "indent"])),_,_,_))
             ) -> ((Some (P.Indent)))
         | ((Node
             (((("Item")[@reason.raw_literal "Item"]),(("full_indent")
               [@reason.raw_literal "full_indent"])),_,_,_)))
             -> ((Some (P.FullIndent)))
         | _ -> None) in
  (name, comment, children)
let parseRule children =
  let name =
    (RU.getContentsByLabel children (("name")[@reason.raw_literal "name"]))
      |> RU.unwrap in
  let (newLines,passThrough,leaf) =
    List.fold_left
      (fun flags  ->
         fun child  ->
           match child with
           | ((("decorators")[@reason.raw_literal "decorators"]),((Node
              (_,children,_,_)))) ->
               let (white,pass,leaf) = flags in
               (match parseDecorator children with
                | ((("ignoreNewlines")[@reason.raw_literal "ignoreNewlines"]),((Bool
                   (whether)))::[]) ->
                    (((match whether with | true  -> P.Yes | false  -> P.No)),
                      pass, leaf)
                | ((("ignoreNewlines")[@reason.raw_literal "ignoreNewlines"]),[])
                    -> (P.Yes, pass, leaf)
                | ((("passThrough")[@reason.raw_literal "passThrough"]),[])
                    -> (white, true, leaf)
                | ((("leaf")[@reason.raw_literal "leaf"]),[]) ->
                    (white, pass, true)
                | ((("lineComment")[@reason.raw_literal "lineComment"]),_)
                  |((("blockComment")[@reason.raw_literal "blockComment"]),_)
                    -> flags
                | (name,_) ->
                    (Printf.eprintf
                       (("Ignoring decorator %s\n")[@reason.raw_literal
                                                     "Ignoring decorator %s\\n"])
                       name;
                     flags))
           | _ -> flags) (P.Inherit, false, false) children in
  (name,
    {
      P.passThrough = passThrough;
      P.docs = None;
      P.ignoreNewlines = newLines;
      capturesComments = false;
      preserveInnerSpace = false;
      P.leaf = leaf;
      P.choices =
        (RU.getChildren children
           (fun (_,child)  ->
              match child with
              | ((Node
                  (((("Choice")[@reason.raw_literal "Choice"]),_),children,_,_))
                  ) ->
                  ((Some ((parseChoice children))))
              | _ -> None))
    })
let getToplevelDecorators children =
  List.fold_left
    (fun decs  ->
       fun child  ->
         match child with
         | ((("decorators")[@reason.raw_literal "decorators"]),((Node
            (_,children,_,_)))) ->
             let (line,block) = decs in
             (match parseDecorator children with
              | ((("lineComment")[@reason.raw_literal "lineComment"]),((String
                 (line)))::[]) ->
                  (((Some (line))), block)
              | ((("blockComment")[@reason.raw_literal "blockComment"]),((String
                 (one)))::((String
                 (two)))::[]) ->
                  (line, ((Some ((one, two)))))
              | _ -> decs)
         | _ -> decs) (None, None) children
let convert (result : result) =
  match result with
  | ((Node
      (((("Start")[@reason.raw_literal "Start"]),_),children,_,_)))
      ->
      let rules =
        RU.getChildren children
          (fun (_,child)  ->
             match child with
             | ((Node
                 (((("Rule")[@reason.raw_literal "Rule"]),_),children,_,_))
                 ) ->
                 ((Some ((parseRule children))))
             | _ -> None) in
      let (lineComment,blockComment) =
        (RU.getChild children
           (fun (label,child)  ->
              match child with
              | ((Node
                  (((("Rule")[@reason.raw_literal "Rule"]),_),children,_,_))
                  ) ->
                  ((Some ((getToplevelDecorators children))))
              | _ -> None))
          |> (optOr (None, None)) in
      { P.lineComment = lineComment; blockComment; rules }
  | _ ->
      failwith
        (("Base must be of type `start`")[@reason.raw_literal
                                           "Base must be of type `start`"])
end
module PackCore
= struct
#1 "PackCore.ml"
[@@@ocaml.ppx.context { cookies = [] }]
module StringSet = Set.Make(String)
type ('result,'errors) lr =
  {
  mutable seed: ('result,'errors) ans;
  mutable rulename: string;
  mutable head: head option;}
and ('result,'errors) memoentry =
  {
  mutable ans: ('result,'errors) ans_or_lr;
  mutable pos: Lexing.position;}
and ('result,'errors) ans_or_lr =
  | Answer of ('result,'errors) ans
  | LR of ('result,'errors) lr
and ('result,'errors) ans = (Lexing.position* 'result* 'errors)
and head =
  {
  mutable hrule: string;
  mutable involved_set: StringSet.t;
  mutable eval_set: StringSet.t;}
module T =
  struct
    type ('result,'errors) state =
      {
      mutable lrstack: ('result,'errors) lr list;
      memo:
        ((StringSet.elt* Lexing.position),('result,'errors) memoentry)
          Hashtbl.t;
      heads: (Lexing.position,head) Hashtbl.t;
      mutable cpos: Lexing.position;
      len: int;
      input: string;}
  end
open T
let initialState input cpos =
  {
    lrstack = [];
    cpos;
    memo = (Hashtbl.create 100);
    heads = (Hashtbl.create 100);
    len = (String.length input);
    input
  }
let tfst (a,_,_) = a
let unwrap opt =
  match opt with
  | ((Some (x))) -> x
  | None  ->
      failwith (("Expected Some(x)")[@reason.raw_literal "Expected Some(x)"])
let show_ans message (pos,_,(epos,errors)) =
  Printf.eprintf
    (("%s :: (%d)\n%s\n")[@reason.raw_literal "%s :: (%d)\\n%s\\n"]) message
    epos.Lexing.pos_cnum (PackTypes.Error.errorsText errors)
let show_ansorlr message ansor =
  match ansor with
  | ((Answer (ans))) -> show_ans message ans
  | ((LR (lr))) ->
      show_ans (message ^ (("[lr seed]")[@reason.raw_literal "[lr seed]"]))
        lr.seed
type ('result,'errors) env =
  {
  state: ('result,'errors) state;
  emptyResult: Lexing.position -> string -> bool -> 'result;
  emptyErrors: 'errors;
  mergeErrors: 'errors -> 'errors -> 'errors;}
let rec apply_rule ~env  ~parse  rulename i ignoringNewlines isNegated path =
  let isLexical = (Char.uppercase (rulename.[0])) <> (rulename.[0]) in
  match recall ~env parse rulename i isLexical ignoringNewlines isNegated
          path
  with
  | None  ->
      let lr =
        {
          seed =
            (Lexing.dummy_pos, (env.emptyResult i rulename isLexical),
              (env.emptyErrors));
          rulename;
          head = None
        } in
      ((env.state).lrstack <- lr :: ((env.state).lrstack);
       (let memoentry = { ans = ((LR (lr))); pos = i } in
        Hashtbl.add (env.state).memo (rulename, i) memoentry;
        (let answer =
           parse env.state rulename i isLexical ignoringNewlines isNegated
             path in
         (env.state).lrstack <- List.tl (env.state).lrstack;
         memoentry.pos <- (env.state).cpos;
         if lr.head <> None
         then
           (lr.seed <- answer;
            lr_answer ~env parse rulename i memoentry isLexical
              ignoringNewlines isNegated path)
         else
           (memoentry.ans <- ((Answer (answer))); answer))))
  | ((Some (memoentry))) ->
      ((env.state).cpos <- memoentry.pos;
       (match memoentry.ans with
        | ((LR (lr))) ->
            (setup_lr ~env rulename lr; lr.seed)
        | ((Answer (answer))) -> answer))
and setup_lr ~env  rulename lr =
  if lr.head = None
  then
    lr.head <-
      ((Some
          ({
             hrule = rulename;
             involved_set = StringSet.empty;
             eval_set = StringSet.empty
           })));
  (let lr_head = unwrap lr.head in
   let rec loop =
     function
     | [] -> ()
     | l::_ when l.head = ((Some (lr_head))) -> ()
     | l::ls ->
         (l.head <- ((Some (lr_head)));
          lr_head.involved_set <-
            StringSet.add l.rulename lr_head.involved_set;
          loop ls) in
   loop (env.state).lrstack)
and lr_answer ~env  parse rulename i memoentry isLexical ignoringNewlines
  isNegated path =
  let lr =
    match memoentry.ans with
    | Answer _ -> assert false
    | ((LR (lr))) -> lr in
  let head =
    match lr.head with
    | None  -> assert false
    | ((Some (head))) -> head in
  if head.hrule <> rulename
  then lr.seed
  else
    (memoentry.ans <- ((Answer ((lr.seed))));
     if (tfst lr.seed) = Lexing.dummy_pos
     then lr.seed
     else
       grow_lr ~env parse rulename i memoentry head isLexical
         ignoringNewlines isNegated path)
and recall ~env  parse rulename i isLexical ignoringNewlines isNegated path =
  let maybeEntry =
    try
      ((Some ((Hashtbl.find (env.state).memo (rulename, i)))))
    with | Not_found  -> None in
  let maybeHead =
    try ((Some ((Hashtbl.find (env.state).heads i))))
    with | Not_found  -> None in
  match maybeHead with
  | None  -> maybeEntry
  | ((Some (head))) ->
      if
        (maybeEntry = None) &&
          (not
             (StringSet.mem rulename
                (StringSet.add head.hrule head.involved_set)))
      then
        ((Some
            ({
               ans =
                 ((Answer
                     ((Lexing.dummy_pos,
                        (env.emptyResult i rulename isLexical),
                        (env.emptyErrors)))));
               pos = i
             })))
      else
        (if StringSet.mem rulename head.eval_set
         then
           (head.eval_set <- StringSet.remove rulename head.eval_set;
            (let answer =
               parse env.state rulename i isLexical ignoringNewlines
                 isNegated path in
             let memoentry = unwrap maybeEntry in
             memoentry.ans <- ((Answer (answer)));
             memoentry.pos <- (env.state).cpos));
         maybeEntry)
and grow_lr ~env  parse rulename i memoentry head isLexical ignoringNewlines
  isNegated path =
  Hashtbl.replace (env.state).heads i head;
  (let rec loop () =
     (env.state).cpos <- i;
     head.eval_set <- head.involved_set;
     (let ans =
        parse env.state rulename i isLexical ignoringNewlines isNegated path in
      let oans =
        match memoentry.ans with
        | ((Answer ((i,_,_)))) -> i
        | LR _ -> Lexing.dummy_pos in
      ((if
          ((tfst ans) = Lexing.dummy_pos) ||
            ((((env.state).cpos).pos_cnum <= (memoentry.pos).pos_cnum) &&
               ((tfst ans).pos_cnum <= oans.pos_cnum))
        then
          ((match memoentry.ans with
            | LR _ -> ()
            | ((Answer ((a,b,oerrs)))) ->
                let (_,_,aerrs) = ans in
                memoentry.ans <-
                  ((Answer ((a, b, (env.mergeErrors oerrs aerrs))))));
           ())
        else
          (memoentry.ans <- ((Answer (ans)));
           memoentry.pos <- (env.state).cpos;
           loop ()))[@ocaml.doc
                      " TODO === all the places were comparing dummy pos "])) in
   loop ();
   Hashtbl.remove (env.state).heads i;
   (env.state).cpos <- memoentry.pos;
   (match memoentry.ans with
    | ((Answer (answer))) -> answer
    | LR _ -> assert false))
end
module RuntimeComments
= struct
#1 "RuntimeComments.ml"
[@@@ocaml.ppx.context { cookies = [] }]
open Lexing
module R = PackTypes.Result
let mkLoc a b = { Location.loc_start = a; loc_end = b; loc_ghost = false }
let incLine pos =
  {
    pos with
    pos_cnum = (pos.pos_cnum + 1);
    pos_lnum = (pos.pos_lnum + 1);
    pos_bol = (pos.pos_cnum + 1)
  }
let rec skipWhite pos text len ignoreNewlines =
  if pos.pos_cnum >= len
  then pos
  else
    (match text.[pos.pos_cnum] with
     | ' ' ->
         skipWhite { pos with pos_cnum = (pos.pos_cnum + 1) } text len
           ignoreNewlines
     | '\t' ->
         skipWhite { pos with pos_cnum = (pos.pos_cnum + 1) } text len
           ignoreNewlines
     | '\n' when ignoreNewlines ->
         skipWhite (incLine pos) text len ignoreNewlines
     | _ -> pos)
let skipALineComment pos start text len =
  let sl = String.length start in
  if
    ((sl + pos.pos_cnum) < len) &&
      ((String.sub text pos.pos_cnum sl) = start)
  then
    try
      let l = String.index_from text pos.pos_cnum '\n' in
      let final =
        {
          pos with
          pos_cnum = (l + 1);
          pos_lnum = (pos.pos_lnum + 1);
          pos_bol = (l + 1)
        } in
      (final,
        ((Some
            (((R.Comment
                 (R.EOL, (String.sub text pos.pos_cnum (l - pos.pos_cnum)),
                   (mkLoc pos final))))))))
    with
    | Not_found  ->
        let final = { pos with pos_cnum = len } in
        (final,
          ((Some
              (((R.Comment
                   (R.EOL,
                     (String.sub text pos.pos_cnum (len - pos.pos_cnum)),
                     (mkLoc pos final))))))))
  else (pos, None)
let skipABlockComment pos (first,last) text len =
  let fl = String.length first in
  if
    ((fl + pos.pos_cnum) < len) &&
      ((String.sub text pos.pos_cnum fl) = first)
  then
    let p0 = pos in
    let fc = last.[0] in
    let ll = String.length last in
    let rec loop pos =
      if (pos.pos_cnum + ll) >= len
      then
        failwith
          (("Unterminated comment")[@reason.raw_literal
                                     "Unterminated comment"])
      else
        if
          ((text.[pos.pos_cnum]) = fc) &&
            ((String.sub text pos.pos_cnum ll) = last)
        then
          (let final = { pos with pos_cnum = (pos.pos_cnum + ll) } in
           (final,
             ((Some
                 (((R.Comment
                      (R.Multi,
                        (String.sub text p0.pos_cnum
                           (final.pos_cnum - p0.pos_cnum)), (mkLoc p0 final)))
                   ))))))
        else
          if (text.[pos.pos_cnum]) = '\n'
          then loop (incLine pos)
          else loop { pos with pos_cnum = (pos.pos_cnum + 1) } in
    loop pos
  else (pos, None)
let rec skipLineComments pos start text len =
  let (pos',contents) = skipALineComment pos start text len in
  match contents with
  | None  -> (pos', [])
  | ((Some (item))) ->
      if (pos'.pos_cnum = len) || (pos.pos_cnum = pos'.pos_cnum)
      then (pos', [item])
      else
        (let pos'' = skipWhite pos' text len true in
         if pos''.pos_cnum > pos'.pos_cnum
         then
           let (p,items) = skipLineComments pos'' start text len in
           (p, (item :: items))
         else (pos'', [item]))
let rec skipBlockComments pos ends text len skipNewlines =
  let (pos',contents) = skipABlockComment pos ends text len in
  match contents with
  | None  -> (pos', [])
  | ((Some (item))) ->
      if (pos'.pos_cnum = len) || (pos.pos_cnum = pos'.pos_cnum)
      then (pos', [item])
      else
        (let pos'' = skipWhite pos' text len skipNewlines in
         if pos''.pos_cnum > pos'.pos_cnum
         then
           let (p,items) = skipBlockComments pos'' ends text len skipNewlines in
           (p, (item :: items))
         else (pos'', [item]))
let rec skipBlockAndLineComments pos ends line text len =
  let (pos',block) = skipABlockComment pos ends text len in
  let pos' = skipWhite pos' text len true in
  let (pos',eol) = skipALineComment pos' line text len in
  if pos'.pos_cnum = pos.pos_cnum
  then (pos', [])
  else
    (let (p,items) = skipBlockAndLineComments pos' ends line text len in
     match (block, eol) with
     | (None ,None ) -> (p, items)
     | (((Some (b))),((Some (l)))) ->
         (p, (b :: l :: items))
     | (None ,((Some (l)))) -> (p, (l :: items))
     | (((Some (b))),None ) -> (p, (b :: items)))
let skipAllWhite pos grammar input len =
  let pos = skipWhite pos input len true in
  match ((grammar.PackTypes.Parsing.blockComment),
          (grammar.PackTypes.Parsing.lineComment))
  with
  | (((Some (x))),None ) ->
      skipBlockComments pos x input len true
  | (((Some (x))),((Some (y)))) ->
      skipBlockAndLineComments pos x y input len
  | (None ,((Some (x)))) ->
      skipLineComments pos x input len
  | (None ,None ) -> (pos, [])
end
module RuntimeUtils
= struct
#1 "RuntimeUtils.ml"
[@@@ocaml.ppx.context { cookies = [] }]
module RP = PackTypes.Path
let rec greedy ~mergeErrors  ~emptyErrors  loop min max subr i path
  greedyCount isNegated =
  match max with
  | ((Some (0))) -> (i, [], emptyErrors)
  | _ ->
      if min > 0
      then
        let (i',found,err) =
          loop i [subr] (((RP.Iter (greedyCount))) :: path)
            0 isNegated in
        (if i' >= i
         then
           let (i'',children,merr) =
             greedy ~mergeErrors ~emptyErrors loop (min - 1) max subr i' path
               (greedyCount + 1) isNegated in
           (i'', (List.concat [found; children]), (mergeErrors err merr))
         else (Lexing.dummy_pos, [], err))
      else
        (let (i',children,err) =
           loop i [subr] (((RP.Iter (greedyCount))) ::
             path) 0 isNegated in
         if i' > i
         then
           let max =
             match max with
             | None  -> None
             | ((Some (n))) ->
                 ((Some ((n - 1)))) in
           let (i'',more,merr) =
             greedy ~mergeErrors ~emptyErrors loop 0 max subr i' path
               (greedyCount + 1) isNegated in
           (i'', (List.concat [children; more]), (mergeErrors err merr))
         else (i, [], err))
end
module Runtime
= struct
#1 "runtime.ml"
[@@@ocaml.ppx.context { cookies = [] }]
module P = PackTypes.Parsing
module R = PackTypes.Result
module RP = PackTypes.Path
[@@@ocaml.doc
  " STATEFUL STUFF\n * It would probably nice to thread this through,\n * but this is way easier at the moment :shrug: "]
let locForOffs a b =
  { Location.loc_start = a; loc_end = b; loc_ghost = false }
let emptyResult pos name isLexical =
  (((R.Leaf ((name, ""), "", (locForOffs pos pos)))),
    false)
let mergeErrors (i1,errs1) (i2,errs2) =
  if i1.Lexing.pos_cnum = i2.Lexing.pos_cnum
  then (i1, (List.concat [errs1; errs2]))
  else if i1.pos_cnum < i2.pos_cnum then (i2, errs2) else (i1, errs1)
let optOr orr opt =
  match opt with | ((Some (x))) -> x | None  -> orr
open PackCore.T
open RuntimeComments
let rec loop ~isLexical  ~state  ~ignoringNewlines  ~grammar  ~parse  i items
  path loopIndex isNegated =
  let loop = loop ~isLexical ~state ~ignoringNewlines ~grammar ~parse in
  let (i,items,comments) =
    if isLexical || (items = [])
    then (i, items, [])
    else
      (match items with
       | ((P.Lexify (p)))::rest -> (i, (p :: rest), [])
       | _ ->
           let i = skipWhite i state.input state.len ignoringNewlines in
           let (i',comments) =
             match (ignoringNewlines, (grammar.P.blockComment),
                     (grammar.P.lineComment))
             with
             | (false ,((Some (x))),_) ->
                 skipBlockComments i x state.input state.len false
             | (true ,((Some (x))),None ) ->
                 skipBlockComments i x state.input state.len true
             | (true ,((Some (x))),((Some
                (y)))) ->
                 skipBlockAndLineComments i x y state.input state.len
             | (true ,None ,((Some (x)))) ->
                 skipLineComments i x state.input state.len
             | (false ,None ,Some _)|(_,None ,None ) -> (i, []) in
           (i', items, comments)) in
  let (pos,results,errors) =
    match items with
    | (P.Empty |P.Indent |P.FullIndent )::rest ->
        loop i rest path (loopIndex + 1) isNegated
    | ((P.NoSpaceAfter (p)))::rest|((P.NoSpaceBefore
      (p)))::rest|((P.NoBreakAfter
      (p)))::rest|((P.NoBreakBefore
      (p)))::rest|((P.Lexify (p)))::rest
        -> loop i (p :: rest) path loopIndex isNegated
    | ((P.Lookahead (p)))::rest ->
        let (i',_,err) = loop i [p] path (loopIndex + 1) isNegated in
        if i' >= i
        then loop i rest path (loopIndex + 1) isNegated
        else (Lexing.dummy_pos, [], err)
    | ((P.Group (g)))::rest ->
        loop i (List.concat [g; rest]) path loopIndex isNegated
    | ((P.Not (p)))::rest ->
        let (i',_,err) =
          loop i [p]
            (((RP.Item (((P.Not (p))), loopIndex))) ::
            path) 0 (not isNegated) in
        if i' >= i
        then (Lexing.dummy_pos, [], err)
        else loop i rest path (loopIndex + 1) isNegated
    | (P.CommentEOL  as item)::rest ->
        (match grammar.P.lineComment with
         | None  ->
             if
               (i.pos_cnum >= state.len) ||
                 (((state.input).[i.pos_cnum]) = '\n')
             then
               let i' = skipWhite i state.input state.len true in
               let (i'',children,rest_errs) =
                 loop i' rest path (loopIndex + 1) isNegated in
               (i'', children, rest_errs)
             else
               (Lexing.dummy_pos, [],
                 (i,
                   [(true, (((RP.Item (item, loopIndex)))
                      :: path))]))
         | ((Some (lineComment))) ->
             let (i',comment) =
               skipLineComments i lineComment state.input state.len in
             let i' =
               if
                 (i'.pos_cnum > i.pos_cnum) ||
                   ((i'.pos_cnum >= state.len) ||
                      (((state.input).[i.pos_cnum]) <> '\n'))
               then i'
               else
                 (let i' = skipWhite i' state.input state.len true in
                  let (i',_comment) =
                    skipLineComments i' lineComment state.input state.len in
                  i') in
             if (i'.pos_cnum > i.pos_cnum) || (i'.pos_cnum >= state.len)
             then
               let (i'',children,rest_errs) =
                 loop i' rest path (loopIndex + 1) isNegated in
               (i'', children, rest_errs)
             else
               (Lexing.dummy_pos, [],
                 (i,
                   [(true, (((RP.Item (item, loopIndex)))
                      :: path))])))
    | (((P.NonTerminal (n,label))) as item)::rest ->
        let env =
          {
            PackCore.state = state;
            emptyResult;
            mergeErrors;
            emptyErrors = (Lexing.dummy_pos, [])
          } in
        let (i',(result,passThrough),errs) =
          PackCore.apply_rule ~env ~parse:(parse grammar) n i
            ignoringNewlines isNegated
            (((RP.Item (item, loopIndex))) :: path) in
        if i' >= i
        then
          let (i'',children,rest_errs) =
            loop i' rest path (loopIndex + 1) isNegated in
          let children =
            match passThrough with
            | true  ->
                (match result with
                 | ((R.Node (_,subchildren,_,_comments)))
                     -> List.concat [subchildren; children]
                 | R.Comment _ ->
                     failwith
                       (("Passthrough can't handle a comment")[@reason.raw_literal
                                                                "Passthrough can't handle a comment"])
                 | R.Leaf _ ->
                     failwith
                       (("Passthrough can't have a leaf node")[@reason.raw_literal
                                                                "Passthrough can't have a leaf node"]))
            | false  -> ((label |> (optOr "")), result) :: children in
          (i'', children, (mergeErrors errs rest_errs))
        else (Lexing.dummy_pos, [], errs)
    | (((P.Terminal (target_string,label))) as item)::rest
        ->
        let slen = String.length target_string in
        if (i.pos_cnum + slen) > state.len
        then
          (Lexing.dummy_pos, [],
            (i,
              [(true, (((RP.Item (item, loopIndex))) ::
                 path))]))
        else
          (let sub = String.sub state.input i.pos_cnum slen in
           if sub = target_string
           then
             let (i'',children,err) =
               loop { i with pos_cnum = (i.pos_cnum + slen) } rest path
                 (loopIndex + 1) isNegated in
             let children =
               match label with
               | ((Some (x))) ->
                   (x,
                     ((R.Leaf
                         (("", target_string), target_string,
                           (locForOffs i
                              { i with pos_cnum = (i.pos_cnum + slen) })))
                     ))
                   :: children
               | None  -> children in
             (i'', children, err)
           else
             (Lexing.dummy_pos, [],
               (i,
                 [(true, (((RP.Item (item, loopIndex))) ::
                    path))])))
    | (((P.Any (label))) as item)::rest ->
        if i.pos_cnum >= state.len
        then
          (Lexing.dummy_pos, [],
            (i,
              [(true, (((RP.Item (item, loopIndex))) ::
                 path))]))
        else
          (let (i'',children,err) =
             loop { i with pos_cnum = (i.pos_cnum + 1) } rest path
               (loopIndex + 1) isNegated in
           let contents = String.sub state.input i.pos_cnum 1 in
           let children =
             match label with
             | ((Some (x))) ->
                 (x,
                   ((R.Leaf
                       (("", contents), contents,
                         (locForOffs i { i with pos_cnum = (i.pos_cnum + 1) })))
                   ))
                 :: children
             | None  -> children in
           (i'', children, err))
    | (P.EOF )::rest ->
        if i.pos_cnum >= state.len
        then (i, [], (Lexing.dummy_pos, []))
        else
          (Lexing.dummy_pos, [],
            (i,
              [(true, (((RP.Item (P.EOF, loopIndex))) ::
                 path))]))
    | (((P.Chars (c1,c2,label))) as item)::rest ->
        if i.pos_cnum >= state.len
        then
          (Lexing.dummy_pos, [],
            (i,
              [(true, (((RP.Item (item, loopIndex))) ::
                 path))]))
        else
          if
            (((state.input).[i.pos_cnum]) >= c1) &&
              (((state.input).[i.pos_cnum]) <= c2)
          then
            (let (i'',children,errs) =
               loop { i with pos_cnum = (i.pos_cnum + 1) } rest path
                 (loopIndex + 1) isNegated in
             let contents = String.sub state.input i.pos_cnum 1 in
             let children =
               match label with
               | ((Some (x))) ->
                   (x,
                     ((R.Leaf
                         (("", contents), contents,
                           (locForOffs i
                              { i with pos_cnum = (i.pos_cnum + 1) })))
                     ))
                   :: children
               | None  -> children in
             (i'', children, errs))
          else
            (Lexing.dummy_pos, [],
              (i,
                [(true, (((RP.Item (item, loopIndex))) ::
                   path))]))
    | (((P.Star (subr))) as item)::rest ->
        let (i',subchildren,errs) =
          RuntimeUtils.greedy ~mergeErrors
            ~emptyErrors:(Lexing.dummy_pos, []) loop 0 None subr i
            (((RP.Item (item, loopIndex))) :: path) 0
            isNegated in
        if i' >= i
        then
          let (i'',children,more_errs) =
            loop i' rest path (loopIndex + 1) isNegated in
          (i'', (List.concat [subchildren; children]),
            (mergeErrors errs more_errs))
        else (Lexing.dummy_pos, [], errs)
    | (((P.Plus (subr))) as item)::rest ->
        let (i',subchildren,errs) =
          RuntimeUtils.greedy ~mergeErrors
            ~emptyErrors:(Lexing.dummy_pos, []) loop 1 None subr i
            (((RP.Item (item, loopIndex))) :: path) 0
            isNegated in
        if i' >= i
        then
          let (i'',children,more_errs) =
            loop i' rest path (loopIndex + 1) isNegated in
          (i'', (List.concat [subchildren; children]),
            (mergeErrors errs more_errs))
        else (Lexing.dummy_pos, [], errs)
    | (((P.Optional (subr))) as item)::rest ->
        let (i',subchildren,errs) =
          RuntimeUtils.greedy ~mergeErrors
            ~emptyErrors:(Lexing.dummy_pos, []) loop 0
            ((Some (1))) subr i
            (((RP.Item (item, loopIndex))) :: path) 0
            isNegated in
        if i' >= i
        then
          let (i'',children,more_errs) =
            loop i' rest path (loopIndex + 1) isNegated in
          (i'', (List.concat [subchildren; children]),
            (mergeErrors errs more_errs))
        else (Lexing.dummy_pos, [], errs)
    | [] -> (i, [], (Lexing.dummy_pos, [])) in
  (pos, ((comments |> (List.map (fun m  -> ("", m)))) @ results), errors)
let rec parse grammar state rulename i isLexical ignoringNewlines isNegated
  path =
  let { P.ignoreNewlines = ignoreNewlines; capturesComments; choices;
        passThrough; leaf }
    =
    try List.assoc rulename grammar.P.rules
    with
    | Not_found  ->
        (Printf.eprintf
           (("error in grammar: unknown rulename '%s'\n")[@reason.raw_literal
                                                           "error in grammar: unknown rulename '%s'\\n"])
           rulename;
         exit 1) in
  let wasIgnoringNewlines = ignoringNewlines in
  let ignoringNewlines =
    match (ignoreNewlines, ignoringNewlines) with
    | (P.Inherit ,x) -> x
    | (P.No ,_) -> false
    | (P.Yes ,_) -> true in
  let numChoices = List.length choices in
  let rec process choices prevErrors choiceIndex =
    match choices with
    | [] ->
        (Lexing.dummy_pos, (emptyResult i rulename isLexical), prevErrors)
    | (sub_name,comment,rs)::otherChoices ->
        let subPath =
          match numChoices == 1 with
          | true  -> path
          | false  -> ((RP.Choice (choiceIndex, sub_name)))
              :: path in
        let (i',children,err) =
          loop ~isLexical ~state ~ignoringNewlines ~grammar ~parse i rs
            subPath 0 isNegated in
        let errs = mergeErrors prevErrors err in
        if i' >= i
        then
          let name = (rulename, sub_name) in
          let loc = locForOffs i i' in
          let result =
            ((match leaf with
              | true  ->
                  ((R.Leaf
                      (name,
                        (String.sub state.input i.pos_cnum
                           (i'.pos_cnum - i.pos_cnum)), loc)))
              | false  ->
                  ((R.Node (name, children, loc, None)))),
              passThrough) in
          (i', result, errs)
        else process otherChoices errs (choiceIndex + 1) in
  process choices (Lexing.dummy_pos, []) 0
let parse ?(filename= (("no name")[@reason.raw_literal "no name"])) 
  (grammar : PackTypes.Parsing.grammar) start input =
  let pos =
    { Lexing.pos_fname = filename; pos_cnum = 0; pos_lnum = 1; pos_bol = 0 } in
  let state = PackCore.initialState input pos in
  let (i,(result,_),errs) =
    PackCore.apply_rule
      ~env:{
             state;
             emptyResult;
             mergeErrors;
             emptyErrors = (Lexing.dummy_pos, [])
           } ~parse:(parse grammar) start pos false false [] in
  let (i,trailingComments) =
    match i.pos_cnum >= 0 with
    | true  -> skipAllWhite i grammar input (String.length input)
    | false  -> (i, []) in
  if i = Lexing.dummy_pos
  then
    ((Belt.Result.Error ((None, (0, (fst errs), errs)))))
  else
    if i.pos_cnum < state.len
    then
      ((Belt.Result.Error
          ((((Some (result))),
             ((i.pos_cnum), (fst errs), errs)))))
    else ((Belt.Result.Ok (result)))
end
module Grammar
= struct
#1 "Grammar.ml"
[@@@ocaml.ppx.context { cookies = [] }]
let choice raw =
  match Runtime.parse GrammarGrammar.grammar
          (("Choice")[@reason.raw_literal "Choice"]) raw
  with
  | ((Error ((maybeResult,(charsParsed,_,failure))))) ->
      (Printf.eprintf (("%s\n")[@reason.raw_literal "%s\\n"])
         (PackTypes.Error.genErrorText raw failure);
       failwith
         (("Unable to parse grammar")[@reason.raw_literal
                                       "Unable to parse grammar"]))
  | ((Belt.Result.Ok
      (((Node (_,children,_,_)))))) ->
      let mid = Unix.gettimeofday () in
      let (_,_,res) = GrammarOfGrammar.parseChoice children in res
  | ((Belt.Result.Ok ((Leaf _|Comment _)))) -> assert false
let getResult grammar entry contents =
  match Runtime.parse grammar entry contents with
  | ((Belt.Result.Error
      ((maybeResult,(charsParsed,_,failure))))) ->
      (Printf.eprintf (("%s\n")[@reason.raw_literal "%s\\n"])
         (PackTypes.Error.genErrorText contents failure);
       exit 10)
  | ((Belt.Result.Ok
      (((Node ((_,sub),children,loc,comments))))))
      -> (sub, children, loc, comments)
  | ((Belt.Result.Ok ((Leaf _|Comment _)))) ->
      failwith
        (("parse should not be a leaf")[@reason.raw_literal
                                         "parse should not be a leaf"])
end
module Infix
= struct
#1 "Infix.ml"
[@@@ocaml.ppx.context { cookies = [] }]
let optMap: ('a -> 'b option) -> 'a list -> 'b list =
  fun fn  ->
    fun items  ->
      List.fold_left
        (fun result  ->
           fun item  ->
             match fn item with
             | None  -> result
             | ((Some (res))) -> res :: result) [] items
  
let (|!) o d =
  match o with | None  -> failwith d | ((Some (v))) -> v
let (|?) o d =
  match o with | None  -> d | ((Some (v))) -> v
let (|??) o d =
  match o with
  | None  -> d
  | ((Some (v))) -> ((Some (v)))
let (|?>) o fn =
  match o with | None  -> None | ((Some (v))) -> fn v
let (|?>>) o fn =
  match o with
  | None  -> None
  | ((Some (v))) -> ((Some ((fn v))))
let fold o d f =
  match o with | None  -> d | ((Some (v))) -> f v
let (|.!) fn message arg = (fn arg) |! message
let (|?<) o fn =
  match o with | None  -> () | ((Some (v))) -> fn v
let fileConcat a b =
  if
    (b <> "") &&
      (((b.[0]) = '.') && (((String.length b) >= 2) && ((b.[1]) = '/')))
  then Filename.concat a (String.sub b 2 ((String.length b) - 2))
  else Filename.concat a b
let logIfAbsent message x =
  match x with
  | None  ->
      (output_string stderr (message ^ (("\n")[@reason.raw_literal "\\n"]));
       None)
  | _ -> x
let maybeConcat a b =
  if (b <> "") && ((b.[0]) = '/') then b else fileConcat a b
let (/+) = fileConcat
end
module LispGrammar
= struct
#1 "lispGrammar.ml"
[@@@ocaml.ppx.context { cookies = [] }]
open Asttypes
open Parsetree
open Longident
module H = Ast_helper
let rec expressionSequence exprs =
  match exprs with
  | [] ->
      H.Exp.construct
        (Location.mknoloc
           ((Lident ((("()")[@reason.raw_literal "()"])))))
        None
  | one::[] -> one
  | one::rest -> H.Exp.sequence one (expressionSequence rest)
let makeArrow ~args  ~body  =
  let rec loop args =
    match args with
    | [] -> body
    | (label,expr,pat)::rest ->
        H.Exp.fun_ ~loc:(pat.ppat_loc) label expr pat (loop rest) in
  loop args
let constructorArgs exprs fn =
  match exprs with | one::[] -> one | _ -> fn exprs
[@@@lineComment ((";")[@reason.raw_literal ";"])]
[@@@blockComment
  ((("(**")[@reason.raw_literal "(**"]), (("*)")[@reason.raw_literal "*)"]))]
let argPat label mtyp =
  match mtyp with
  | None  -> H.Pat.var label
  | ((Some (t))) -> H.Pat.constraint_ (H.Pat.var label) t
let rec listToConstruct ~loc  list maybeRest construct tuple itemLoc =
  match list with
  | [] ->
      (match maybeRest with
       | None  ->
           construct ~loc
             (Location.mkloc
                ((Lident ((("[]")[@reason.raw_literal "[]"])))) loc) None
       | ((Some (x))) -> x)
  | one::rest ->
      construct ~loc:(itemLoc one)
        (Location.mkloc
           ((Lident ((("::")[@reason.raw_literal "::"]))))
           Location.none)
        ((Some
            ((tuple
                [one;
                listToConstruct ~loc:(itemLoc one) rest maybeRest construct
                  tuple itemLoc]))))
let stripQuotes str = String.sub str 1 ((String.length str) - 2)
let processString str = (str |> stripQuotes) |> Scanf.unescaped
let rec convert_opChar (sub,children,_loc,_comments) =
  match sub with | _ -> assert false
and convert_reservedOps (sub,children,_loc,_comments) =
  match sub with | _ -> assert false
and convert_alpha (sub,children,_loc,_comments) =
  match sub with | _ -> assert false
and convert_reserved (sub,children,_loc,_comments) =
  match sub with | _ -> assert false
and convert_charchar (sub,children,_loc,_comments) =
  match sub with | _ -> assert false
and convert_strchar (sub,children,_loc,_comments) =
  match sub with | _ -> assert false
and convert_ConstString (sub,children,_loc,_comments) =
  let (((t,loc))[@text (("string")[@reason.raw_literal "string"])]) =
    match ResultUtils.getLeafByType children "string" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "ConstString", "string"))
          )
    | ((Some ((contents,loc)))) -> (contents, loc) in
  ((Const_string ((processString t), None)))
and convert_identchar (sub,children,_loc,_comments) =
  match sub with | _ -> assert false
and convert_polyIdent (sub,children,_loc,_comments) =
  let (((name,loc))[@text (("capIdent")[@reason.raw_literal "capIdent"])]) =
    match ResultUtils.getLeafByType children "capIdent" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "polyIdent", "capIdent"))
          )
    | ((Some ((contents,loc)))) -> (contents, loc) in
  Location.mkloc name loc
and convert_constant (sub,children,_loc,_comments) =
  match sub with
  | "float" ->
      let (((t,_))[@text (("float")[@reason.raw_literal "float"])]) =
        match ResultUtils.getLeafByType children "float" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "constant:float", "float"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      ((Const_float (t)))
  | "int" ->
      let (((t,_))[@text (("int64")[@reason.raw_literal "int64"])]) =
        match ResultUtils.getLeafByType children "int64" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "constant:int", "int64"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      ((Const_int ((int_of_string t))))
  | "string" ->
      let ((t)[@node (("ConstString")[@reason.raw_literal "ConstString"])]) =
        match ResultUtils.getNodeByType children "ConstString" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "constant:string", "ConstString")))
        | ((Some (node))) -> convert_ConstString node in
      t
  | "longString" ->
      let (((t,_))[@text (("longString")[@reason.raw_literal "longString"])])
        =
        match ResultUtils.getLeafByType children "longString" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "constant:longString", "longString")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      ((Const_string
          ((String.sub t 2 ((String.length t) - 4)),
            ((Some (""))))))
  | "char" ->
      let (((t,_))[@text (("char")[@reason.raw_literal "char"])]) =
        match ResultUtils.getLeafByType children "char" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "constant:char", "char"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      ((Const_char ((t.[0]))))
  | _ -> assert false
and convert_decoratorChar (sub,children,_loc,_comments) =
  match sub with | _ -> assert false
and convert_longCap_ (sub,children,_loc,_comments) =
  match sub with
  | "dot" ->
      let loc = _loc in
      let (((base,_))[@node (("longCap_")[@reason.raw_literal "longCap_"])])
        =
        match ResultUtils.getNodeByType children "longCap_" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "longCap_:dot", "longCap_"))
              )
        | ((Some (node))) -> convert_longCap_ node in
      let (((text,_))[@text (("capIdent")[@reason.raw_literal "capIdent"])])
        =
        match ResultUtils.getLeafByType children "capIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "longCap_:dot", "capIdent"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      (((Ldot (base, text))), loc)
  | "lident" ->
      let (((text,loc))[@text (("capIdent")[@reason.raw_literal "capIdent"])])
        =
        match ResultUtils.getLeafByType children "capIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "longCap_:lident", "capIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      (((Lident (text))), loc)
  | _ -> assert false
and convert_longCap (sub,children,_loc,_comments) =
  let (((l,loc))[@node (("longCap_")[@reason.raw_literal "longCap_"])]) =
    match ResultUtils.getNodeByType children "longCap_" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "longCap", "longCap_")))
    | ((Some (node))) -> convert_longCap_ node in
  Location.mkloc l loc
and convert_longIdent (sub,children,_loc,_comments) =
  let loc = _loc in
  let ((base)[@node_opt (("longCap_")[@reason.raw_literal "longCap_"])]) =
    match ResultUtils.getNodeByType children "longCap_" with
    | None  -> None
    | ((Some (node))) ->
        ((Some ((convert_longCap_ node)))) in
  let (((text,_))[@text (("lowerIdent")[@reason.raw_literal "lowerIdent"])])
    =
    match ResultUtils.getLeafByType children "lowerIdent" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "longIdent", "lowerIdent"))
          )
    | ((Some ((contents,loc)))) -> (contents, loc) in
  match base with
  | None  -> Location.mkloc ((Lident (text))) loc
  | ((Some ((base,_loc)))) ->
      Location.mkloc ((Ldot (base, text))) loc
and convert_attribute (sub,children,_loc,_comments) =
  let ((ident)[@node (("longIdent")[@reason.raw_literal "longIdent"])]) =
    match ResultUtils.getNodeByType children "longIdent" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "attribute", "longIdent"))
          )
    | ((Some (node))) -> convert_longIdent node in
  ident
and convert_argLabel (sub,children,_loc,_comments) =
  let (((text,loc))[@text (("lowerIdent")[@reason.raw_literal "lowerIdent"])])
    =
    match ResultUtils.getLeafByType children "lowerIdent" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "argLabel", "lowerIdent"))
          )
    | ((Some ((contents,loc)))) -> (contents, loc) in
  Location.mkloc text loc
and convert_argLabelWithConstraint (sub,children,_loc,_comments) =
  let loc = _loc in
  let ((ident)[@node (("argLabel")[@reason.raw_literal "argLabel"])]) =
    match ResultUtils.getNodeByType children "argLabel" with
    | None  ->
        raise
          ((PackTypes.ConversionError
              (_loc, "argLabelWithConstraint", "argLabel")))
    | ((Some (node))) -> convert_argLabel node in
  let ((typ)[@node_opt (("CoreType")[@reason.raw_literal "CoreType"])]) =
    match ResultUtils.getNodeByType children "CoreType" with
    | None  -> None
    | ((Some (node))) ->
        ((Some ((convert_CoreType node)))) in
  (ident, typ)
and convert_PatternObjectItem (sub,children,_loc,_comments) =
  match sub with
  | "normal" ->
      let loc = _loc in
      let ((attr)[@node (("attribute")[@reason.raw_literal "attribute"])]) =
        match ResultUtils.getNodeByType children "attribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "PatternObjectItem:normal", "attribute")))
        | ((Some (node))) -> convert_attribute node in
      let ((pattern)[@node (("Pattern")[@reason.raw_literal "Pattern"])]) =
        match ResultUtils.getNodeByType children "Pattern" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "PatternObjectItem:normal", "Pattern")))
        | ((Some (node))) -> convert_Pattern node in
      (attr, pattern)
  | "punned" ->
      let loc = _loc in
      let ((attr)[@node (("attribute")[@reason.raw_literal "attribute"])]) =
        match ResultUtils.getNodeByType children "attribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "PatternObjectItem:punned", "attribute")))
        | ((Some (node))) -> convert_attribute node in
      (attr, (H.Pat.var (Location.mkloc (Longident.last attr.txt) attr.loc)))
  | _ -> assert false
and convert_FnArg (sub,children,_loc,_comments) =
  match sub with
  | "destructured" ->
      let loc = _loc in
      let (((label,mtyp))[@node
                           (("argLabelWithConstraint")[@reason.raw_literal
                                                        "argLabelWithConstraint"])])
        =
        match ResultUtils.getNodeByType children "argLabelWithConstraint"
        with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnArg:destructured", "argLabelWithConstraint"))
              )
        | ((Some (node))) ->
            convert_argLabelWithConstraint node in
      let ((pattern)[@node (("Pattern")[@reason.raw_literal "Pattern"])]) =
        match ResultUtils.getNodeByType children "Pattern" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnArg:destructured", "Pattern")))
        | ((Some (node))) -> convert_Pattern node in
      ((label.txt), None,
        ((match mtyp with
          | None  -> pattern
          | ((Some (mtyp))) ->
              H.Pat.constraint_ pattern mtyp)))
  | "optional" ->
      let loc = _loc in
      let (((label,mtyp))[@node
                           (("argLabelWithConstraint")[@reason.raw_literal
                                                        "argLabelWithConstraint"])])
        =
        match ResultUtils.getNodeByType children "argLabelWithConstraint"
        with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnArg:optional", "argLabelWithConstraint"))
              )
        | ((Some (node))) ->
            convert_argLabelWithConstraint node in
      (((("?")[@reason.raw_literal "?"]) ^ label.txt), None,
        (argPat label mtyp))
  | "defaulted" ->
      let loc = _loc in
      let (((label,mtyp))[@node
                           (("argLabelWithConstraint")[@reason.raw_literal
                                                        "argLabelWithConstraint"])])
        =
        match ResultUtils.getNodeByType children "argLabelWithConstraint"
        with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnArg:defaulted", "argLabelWithConstraint"))
              )
        | ((Some (node))) ->
            convert_argLabelWithConstraint node in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnArg:defaulted", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      ((label.txt), ((Some (expr))), (argPat label mtyp))
  | "labeled" ->
      let loc = _loc in
      let (((label,mtyp))[@node
                           (("argLabelWithConstraint")[@reason.raw_literal
                                                        "argLabelWithConstraint"])])
        =
        match ResultUtils.getNodeByType children "argLabelWithConstraint"
        with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnArg:labeled", "argLabelWithConstraint")))
        | ((Some (node))) ->
            convert_argLabelWithConstraint node in
      ((label.txt), None, (argPat label mtyp))
  | "unlabeled" ->
      let loc = _loc in
      let ((pattern)[@node (("Pattern")[@reason.raw_literal "Pattern"])]) =
        match ResultUtils.getNodeByType children "Pattern" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnArg:unlabeled", "Pattern")))
        | ((Some (node))) -> convert_Pattern node in
      ("", None, pattern)
  | _ -> assert false
and convert_FnArgs (sub,children,_loc,_comments) =
  match sub with
  | "single" ->
      let (((text,loc))[@text
                         (("lowerIdent")[@reason.raw_literal "lowerIdent"])])
        =
        match ResultUtils.getLeafByType children "lowerIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnArgs:single", "lowerIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      [("", None, (H.Pat.var (Location.mkloc text loc)))]
  | "unit" ->
      let loc = _loc in
      [("", None,
         (H.Pat.var (Location.mkloc (("()")[@reason.raw_literal "()"]) loc)))]
  | "ignored" -> let loc = _loc in [("", None, (H.Pat.any ~loc ()))]
  | "multiple" ->
      let loc = _loc in
      let ((args)[@nodes (("FnArg")[@reason.raw_literal "FnArg"])]) =
        ResultUtils.getNodesByType children "FnArg" convert_FnArg in
      args
  | _ -> assert false
and convert_ObjectItem (sub,children,_loc,_comments) =
  match sub with
  | "normal" ->
      let loc = _loc in
      let ((attr)[@node (("attribute")[@reason.raw_literal "attribute"])]) =
        match ResultUtils.getNodeByType children "attribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ObjectItem:normal", "attribute")))
        | ((Some (node))) -> convert_attribute node in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ObjectItem:normal", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      (attr, expr)
  | "punned" ->
      let loc = _loc in
      let ((attr)[@node (("attribute")[@reason.raw_literal "attribute"])]) =
        match ResultUtils.getNodeByType children "attribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ObjectItem:punned", "attribute")))
        | ((Some (node))) -> convert_attribute node in
      (attr,
        (H.Exp.ident
           (Location.mkloc
              ((Lident ((Longident.last attr.txt))))
              attr.loc)))
  | _ -> assert false
and convert_ThreadItem (sub,children,_loc,_comments) =
  match sub with
  | "attribute" ->
      let loc = _loc in
      let ((attr)[@node (("attribute")[@reason.raw_literal "attribute"])]) =
        match ResultUtils.getNodeByType children "attribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ThreadItem:attribute", "attribute")))
        | ((Some (node))) -> convert_attribute node in
      (loc, (`Attribute attr))
  | "ident" ->
      let loc = _loc in
      let ((ident)[@node (("longIdent")[@reason.raw_literal "longIdent"])]) =
        match ResultUtils.getNodeByType children "longIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ThreadItem:ident", "longIdent")))
        | ((Some (node))) -> convert_longIdent node in
      (loc, (`Fn ((H.Exp.ident ~loc ident), [])))
  | "emptyconstr" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ThreadItem:emptyconstr", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      (loc, (`Construct (ident, [])))
  | "constructor" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ThreadItem:constructor", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      let ((args)[@nodes (("Expression")[@reason.raw_literal "Expression"])])
        = ResultUtils.getNodesByType children "Expression" convert_Expression in
      (loc, (`Construct (ident, args)))
  | "fn_call" ->
      let loc = _loc in
      let ((fn)[@node.fn (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "fn" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ThreadItem:fn_call", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let ((args)[@nodes.args
                   (("FnCallArg")[@reason.raw_literal "FnCallArg"])])
        = ResultUtils.getNodesByLabel children "args" convert_FnCallArg in
      (loc, (`Fn (fn, args)))
  | _ -> assert false
and convert_SwitchCond (sub,children,_loc,_comments) =
  let loc = _loc in
  let ((pattern)[@node (("Pattern")[@reason.raw_literal "Pattern"])]) =
    match ResultUtils.getNodeByType children "Pattern" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "SwitchCond", "Pattern"))
          )
    | ((Some (node))) -> convert_Pattern node in
  let ((guard)[@node_opt (("Expression")[@reason.raw_literal "Expression"])])
    =
    match ResultUtils.getNodeByType children "Expression" with
    | None  -> None
    | ((Some (node))) ->
        ((Some ((convert_Expression node)))) in
  (pattern, guard)
and convert_SwitchCase (sub,children,_loc,_comments) =
  let loc = _loc in
  let (((pat,guard))[@node
                      (("SwitchCond")[@reason.raw_literal "SwitchCond"])])
    =
    match ResultUtils.getNodeByType children "SwitchCond" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "SwitchCase", "SwitchCond"))
          )
    | ((Some (node))) -> convert_SwitchCond node in
  let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])]) =
    match ResultUtils.getNodeByType children "Expression" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "SwitchCase", "Expression"))
          )
    | ((Some (node))) -> convert_Expression node in
  H.Exp.case pat ?guard expr
and convert_FnCallArg (sub,children,_loc,_comments) =
  match sub with
  | "labeled" ->
      let ((label)[@node (("argLabel")[@reason.raw_literal "argLabel"])]) =
        match ResultUtils.getNodeByType children "argLabel" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnCallArg:labeled", "argLabel")))
        | ((Some (node))) -> convert_argLabel node in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnCallArg:labeled", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      ((label.txt), expr)
  | "punned" ->
      let ((label)[@node (("argLabel")[@reason.raw_literal "argLabel"])]) =
        match ResultUtils.getNodeByType children "argLabel" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnCallArg:punned", "argLabel")))
        | ((Some (node))) -> convert_argLabel node in
      ((label.txt),
        (H.Exp.ident
           (Location.mkloc ((Lident ((label.txt))))
              label.loc)))
  | "expr" ->
      let ((exp)[@node (("Expression")[@reason.raw_literal "Expression"])]) =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "FnCallArg:expr", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      ("", exp)
  | _ -> assert false
and convert_ValueBinding (sub,children,_loc,_comments) =
  let loc = _loc in
  let ((pat)[@node (("Pattern")[@reason.raw_literal "Pattern"])]) =
    match ResultUtils.getNodeByType children "Pattern" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "ValueBinding", "Pattern"))
          )
    | ((Some (node))) -> convert_Pattern node in
  let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])]) =
    match ResultUtils.getNodeByType children "Expression" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "ValueBinding", "Expression"))
          )
    | ((Some (node))) -> convert_Expression node in
  H.Vb.mk ~loc pat expr
and convert_CoreType (sub,children,_loc,_comments) =
  match sub with
  | "constr_no_args" ->
      let loc = _loc in
      let ((ident)[@node (("longIdent")[@reason.raw_literal "longIdent"])]) =
        match ResultUtils.getNodeByType children "longIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "CoreType:constr_no_args", "longIdent")))
        | ((Some (node))) -> convert_longIdent node in
      H.Typ.constr ~loc ident []
  | "variable" ->
      let (((name,loc))[@text
                         (("typeVariable")[@reason.raw_literal
                                            "typeVariable"])])
        =
        match ResultUtils.getLeafByType children "typeVariable" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "CoreType:variable", "typeVariable")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      H.Typ.var ~loc name
  | "constructor" ->
      let loc = _loc in
      let ((ident)[@node (("longIdent")[@reason.raw_literal "longIdent"])]) =
        match ResultUtils.getNodeByType children "longIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "CoreType:constructor", "longIdent")))
        | ((Some (node))) -> convert_longIdent node in
      let ((args)[@nodes (("CoreType")[@reason.raw_literal "CoreType"])]) =
        ResultUtils.getNodesByType children "CoreType" convert_CoreType in
      H.Typ.constr ~loc ident args
  | "arrow" ->
      let loc = _loc in
      let ((args)[@node.args (("CoreType")[@reason.raw_literal "CoreType"])])
        =
        match ResultUtils.getNodeByLabel children "args" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "CoreType:arrow", "CoreType")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_CoreType (sub, children, loc, comments) in
      let ((res)[@node (("CoreType")[@reason.raw_literal "CoreType"])]) =
        match ResultUtils.getNodeByType children "CoreType" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "CoreType:arrow", "CoreType")))
        | ((Some (node))) -> convert_CoreType node in
      H.Typ.arrow "" args res
  | _ -> assert false
and convert_TypeConstructor (sub,children,_loc,_comments) =
  match sub with
  | "no_args" ->
      let loc = _loc in
      let (((text,tloc))[@text
                          (("capIdent")[@reason.raw_literal "capIdent"])])
        =
        match ResultUtils.getLeafByType children "capIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "TypeConstructor:no_args", "capIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      H.Type.constructor ~loc (Location.mkloc text tloc)
  | "args" ->
      let loc = _loc in
      let (((text,tloc))[@text
                          (("capIdent")[@reason.raw_literal "capIdent"])])
        =
        match ResultUtils.getLeafByType children "capIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "TypeConstructor:args", "capIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((args)[@nodes (("CoreType")[@reason.raw_literal "CoreType"])]) =
        ResultUtils.getNodesByType children "CoreType" convert_CoreType in
      H.Type.constructor ~loc ~args (Location.mkloc text tloc)
  | _ -> assert false
and convert_shortAttribute (sub,children,_loc,_comments) =
  let ((pair)[@text (("lowerIdent")[@reason.raw_literal "lowerIdent"])]) =
    match ResultUtils.getLeafByType children "lowerIdent" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "shortAttribute", "lowerIdent"))
          )
    | ((Some ((contents,loc)))) -> (contents, loc) in
  pair
and convert_TypeObjectItem (sub,children,_loc,_comments) =
  match sub with
  | "normal" ->
      let loc = _loc in
      let (((name,nameLoc))[@node
                             (("shortAttribute")[@reason.raw_literal
                                                  "shortAttribute"])])
        =
        match ResultUtils.getNodeByType children "shortAttribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "TypeObjectItem:normal", "shortAttribute")))
        | ((Some (node))) -> convert_shortAttribute node in
      let ((t)[@node (("CoreType")[@reason.raw_literal "CoreType"])]) =
        match ResultUtils.getNodeByType children "CoreType" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "TypeObjectItem:normal", "CoreType")))
        | ((Some (node))) -> convert_CoreType node in
      H.Type.field ~loc (Location.mkloc name nameLoc) t
  | "punned" ->
      let loc = _loc in
      let (((name,nameLoc))[@node
                             (("shortAttribute")[@reason.raw_literal
                                                  "shortAttribute"])])
        =
        match ResultUtils.getNodeByType children "shortAttribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "TypeObjectItem:punned", "shortAttribute")))
        | ((Some (node))) -> convert_shortAttribute node in
      H.Type.field ~loc (Location.mkloc name nameLoc)
        (H.Typ.constr ~loc:nameLoc
           (Location.mkloc ((Lident (name))) nameLoc) [])
  | _ -> assert false
and convert_TypeKind (sub,children,_loc,_comments) =
  match sub with
  | "record" ->
      let loc = _loc in
      let ((items)[@nodes
                    (("TypeObjectItem")[@reason.raw_literal "TypeObjectItem"])])
        =
        ResultUtils.getNodesByType children "TypeObjectItem"
          convert_TypeObjectItem in
      `Kind ((Ptype_record (items)))
  | "constructors" ->
      let loc = _loc in
      let ((decls)[@nodes
                    (("TypeConstructor")[@reason.raw_literal
                                          "TypeConstructor"])])
        =
        ResultUtils.getNodesByType children "TypeConstructor"
          convert_TypeConstructor in
      `Kind ((Ptype_variant (decls)))
  | "alias" ->
      let ((t)[@node (("CoreType")[@reason.raw_literal "CoreType"])]) =
        match ResultUtils.getNodeByType children "CoreType" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "TypeKind:alias", "CoreType")))
        | ((Some (node))) -> convert_CoreType node in
      `Manifest t
  | _ -> assert false
and convert_TypeName (sub,children,_loc,_comments) =
  match sub with
  | "vbl" ->
      let loc = _loc in
      let (((name,loc))[@text
                         (("lowerIdent")[@reason.raw_literal "lowerIdent"])])
        =
        match ResultUtils.getLeafByType children "lowerIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "TypeName:vbl", "lowerIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((vbls)[@texts
                   (("typeVariable")[@reason.raw_literal "typeVariable"])])
        = ResultUtils.getLeafsByType children "typeVariable" in
      ((Location.mkloc name loc),
        (vbls |>
           (List.map (fun (name,loc)  -> ((H.Typ.var ~loc name), Invariant)))))
  | "plain" ->
      let loc = _loc in
      let (((name,loc))[@text
                         (("lowerIdent")[@reason.raw_literal "lowerIdent"])])
        =
        match ResultUtils.getLeafByType children "lowerIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "TypeName:plain", "lowerIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      ((Location.mkloc name loc), [])
  | _ -> assert false
and convert_TypePair (sub,children,_loc,_comments) =
  let loc = _loc in
  let (((name,vbls))[@node (("TypeName")[@reason.raw_literal "TypeName"])]) =
    match ResultUtils.getNodeByType children "TypeName" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "TypePair", "TypeName"))
          )
    | ((Some (node))) -> convert_TypeName node in
  let ((kind)[@node (("TypeKind")[@reason.raw_literal "TypeKind"])]) =
    match ResultUtils.getNodeByType children "TypeKind" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "TypePair", "TypeKind"))
          )
    | ((Some (node))) -> convert_TypeKind node in
  match kind with
  | `Kind kind -> H.Type.mk ~loc ~params:vbls ~kind name
  | `Manifest manifest -> H.Type.mk ~loc ~params:vbls ~manifest name
and convert_LetPair (sub,children,_loc,_comments) =
  let ((pattern)[@node (("Pattern")[@reason.raw_literal "Pattern"])]) =
    match ResultUtils.getNodeByType children "Pattern" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "LetPair", "Pattern")))
    | ((Some (node))) -> convert_Pattern node in
  let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])]) =
    match ResultUtils.getNodeByType children "Expression" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "LetPair", "Expression"))
          )
    | ((Some (node))) -> convert_Expression node in
  H.Vb.mk pattern expr
and convert_ModuleExpr (sub,children,_loc,_comments) =
  match sub with
  | "structure" ->
      let loc = _loc in
      let ((items)[@nodes (("Structure")[@reason.raw_literal "Structure"])])
        = ResultUtils.getNodesByType children "Structure" convert_Structure in
      H.Mod.mk ~loc ((Pmod_structure (items)))
  | "ident" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ModuleExpr:ident", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      H.Mod.mk ~loc ((Pmod_ident (ident)))
  | _ -> assert false
and convert_ModuleApply (sub,children,_loc,_comments) =
  match sub with
  | "ident" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "ModuleApply:ident", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      H.Mod.mk ~loc ((Pmod_ident (ident)))
  | _ -> assert false
and convert_Pattern (sub,children,_loc,_comments) =
  match sub with
  | "ident" ->
      let loc = _loc in
      let (((text,tloc))[@text
                          (("lowerIdent")[@reason.raw_literal "lowerIdent"])])
        =
        match ResultUtils.getLeafByType children "lowerIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Pattern:ident", "lowerIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      H.Pat.var ~loc (Location.mkloc text tloc)
  | "interval" ->
      let loc = _loc in
      let ((f)[@node.f (("constant")[@reason.raw_literal "constant"])]) =
        match ResultUtils.getNodeByLabel children "f" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Pattern:interval", "constant")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_constant (sub, children, loc, comments) in
      let ((s)[@node.s (("constant")[@reason.raw_literal "constant"])]) =
        match ResultUtils.getNodeByLabel children "s" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Pattern:interval", "constant")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_constant (sub, children, loc, comments) in
      H.Pat.interval ~loc f s
  | "constant" ->
      let loc = _loc in
      let ((const)[@node (("constant")[@reason.raw_literal "constant"])]) =
        match ResultUtils.getNodeByType children "constant" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Pattern:constant", "constant")))
        | ((Some (node))) -> convert_constant node in
      H.Pat.constant ~loc const
  | "unit" ->
      let loc = _loc in
      H.Pat.construct ~loc
        (Location.mkloc
           ((Lident ((("()")[@reason.raw_literal "()"]))))
           loc) None
  | "ignored" -> let loc = _loc in H.Pat.any ~loc ()
  | "array" ->
      let loc = _loc in
      let ((items)[@nodes.items (("Pattern")[@reason.raw_literal "Pattern"])])
        = ResultUtils.getNodesByLabel children "items" convert_Pattern in
      let ((spread)[@node_opt.spread
                     (("Pattern")[@reason.raw_literal "Pattern"])])
        =
        match ResultUtils.getNodeByLabel children "spread" with
        | None  -> None
        | ((Some (((_,sub),children,loc,comments)))) ->
            ((Some ((convert_Pattern (sub, children, loc, comments))))
            ) in
      listToConstruct ~loc items spread
        (fun ~loc  -> fun a  -> fun b  -> H.Pat.construct ~loc a b)
        H.Pat.tuple (fun item  -> item.ppat_loc)
  | "tuple" ->
      let loc = _loc in
      let ((patterns)[@nodes (("Pattern")[@reason.raw_literal "Pattern"])]) =
        ResultUtils.getNodesByType children "Pattern" convert_Pattern in
      H.Pat.tuple ~loc patterns
  | "empty_constr" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Pattern:empty_constr", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      H.Pat.construct ~loc ident None
  | "poly" ->
      let loc = _loc in
      let ((ident)[@node (("polyIdent")[@reason.raw_literal "polyIdent"])]) =
        match ResultUtils.getNodeByType children "polyIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "Pattern:poly", "polyIdent"))
              )
        | ((Some (node))) -> convert_polyIdent node in
      let ((args)[@nodes (("Pattern")[@reason.raw_literal "Pattern"])]) =
        ResultUtils.getNodesByType children "Pattern" convert_Pattern in
      H.Pat.variant ~loc ident.txt
        ((Some ((constructorArgs args H.Pat.tuple))))
  | "empty_poly" ->
      let loc = _loc in
      let ((ident)[@node (("polyIdent")[@reason.raw_literal "polyIdent"])]) =
        match ResultUtils.getNodeByType children "polyIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Pattern:empty_poly", "polyIdent")))
        | ((Some (node))) -> convert_polyIdent node in
      H.Pat.variant ~loc ident.txt None
  | "exception" ->
      let loc = _loc in
      let ((arg)[@node (("Pattern")[@reason.raw_literal "Pattern"])]) =
        match ResultUtils.getNodeByType children "Pattern" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Pattern:exception", "Pattern")))
        | ((Some (node))) -> convert_Pattern node in
      H.Pat.exception_ arg
  | "constructor" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Pattern:constructor", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      let ((args)[@nodes (("Pattern")[@reason.raw_literal "Pattern"])]) =
        ResultUtils.getNodesByType children "Pattern" convert_Pattern in
      H.Pat.construct ~loc ident
        ((Some ((constructorArgs args H.Pat.tuple))))
  | "object" ->
      let loc = _loc in
      let ((items)[@nodes
                    (("PatternObjectItem")[@reason.raw_literal
                                            "PatternObjectItem"])])
        =
        ResultUtils.getNodesByType children "PatternObjectItem"
          convert_PatternObjectItem in
      H.Pat.record ~loc items Open
  | "or" ->
      let loc = _loc in
      let ((opts)[@nodes (("Pattern")[@reason.raw_literal "Pattern"])]) =
        ResultUtils.getNodesByType children "Pattern" convert_Pattern in
      let rec loop opts =
        match opts with
        | [] -> assert false
        | one::[] -> one
        | one::rest -> H.Pat.or_ ~loc one (loop rest) in
      loop opts
  | _ -> assert false
and convert_LetPairs (sub,children,_loc,_comments) =
  let ((bindings)[@nodes
                   (("ValueBinding")[@reason.raw_literal "ValueBinding"])])
    = ResultUtils.getNodesByType children "ValueBinding" convert_ValueBinding in
  bindings
and convert_Expression (sub,children,_loc,_comments) =
  match sub with
  | "ident" ->
      let loc = _loc in
      let ((ident)[@node (("longIdent")[@reason.raw_literal "longIdent"])]) =
        match ResultUtils.getNodeByType children "longIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:ident", "longIdent")))
        | ((Some (node))) -> convert_longIdent node in
      H.Exp.ident ~loc ident
  | "const" ->
      let loc = _loc in
      let ((c)[@node (("constant")[@reason.raw_literal "constant"])]) =
        match ResultUtils.getNodeByType children "constant" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:const", "constant")))
        | ((Some (node))) -> convert_constant node in
      H.Exp.constant ~loc c
  | "unit" ->
      let loc = _loc in
      H.Exp.construct ~loc
        (Location.mkloc
           ((Lident ((("()")[@reason.raw_literal "()"]))))
           loc) None
  | "extension_expr" ->
      let loc = _loc in
      let (((text,loc))[@text
                         (("decoratorName")[@reason.raw_literal
                                             "decoratorName"])])
        =
        match ResultUtils.getLeafByType children "decoratorName" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:extension_expr", "decoratorName"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((inner)[@nodes (("Structure")[@reason.raw_literal "Structure"])])
        = ResultUtils.getNodesByType children "Structure" convert_Structure in
      H.Exp.extension ~loc
        ((Location.mkloc text loc), ((PStr (inner))))
  | "decorator_expr_nopayload" ->
      let loc = _loc in
      let (((text,loc))[@text
                         (("decoratorName")[@reason.raw_literal
                                             "decoratorName"])])
        =
        match ResultUtils.getLeafByType children "decoratorName" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:decorator_expr_nopayload",
                    "decoratorName")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((inner)[@node.inner
                    (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "inner" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:decorator_expr_nopayload", "Expression"))
              )
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let attr = ((Location.mkloc text loc), ((PStr ([])))) in
      { inner with pexp_attributes = (attr :: (inner.pexp_attributes)) }
  | "decorator_expr" ->
      let loc = _loc in
      let (((text,loc))[@text
                         (("decoratorName")[@reason.raw_literal
                                             "decoratorName"])])
        =
        match ResultUtils.getLeafByType children "decoratorName" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:decorator_expr", "decoratorName"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((payload)[@node.payload
                      (("Structure")[@reason.raw_literal "Structure"])])
        =
        match ResultUtils.getNodeByLabel children "payload" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:decorator_expr", "Structure")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Structure (sub, children, loc, comments) in
      let ((inner)[@node.inner
                    (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "inner" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:decorator_expr", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let attr =
        ((Location.mkloc text loc), ((PStr ([payload])))) in
      { inner with pexp_attributes = (attr :: (inner.pexp_attributes)) }
  | "constructor" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:constructor", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      let ((exprs)[@nodes (("Expression")[@reason.raw_literal "Expression"])])
        = ResultUtils.getNodesByType children "Expression" convert_Expression in
      H.Exp.construct ~loc ident
        ((Some ((constructorArgs exprs (H.Exp.tuple ~loc)))))
  | "empty_constr" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:empty_constr", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      H.Exp.construct ~loc ident None
  | "constructor_poly" ->
      let loc = _loc in
      let ((ident)[@node (("polyIdent")[@reason.raw_literal "polyIdent"])]) =
        match ResultUtils.getNodeByType children "polyIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:constructor_poly", "polyIdent"))
              )
        | ((Some (node))) -> convert_polyIdent node in
      let ((exprs)[@nodes (("Expression")[@reason.raw_literal "Expression"])])
        = ResultUtils.getNodesByType children "Expression" convert_Expression in
      H.Exp.variant ~loc ident.txt
        ((Some ((constructorArgs exprs H.Exp.tuple))))
  | "empty_poly" ->
      let loc = _loc in
      let ((ident)[@node (("polyIdent")[@reason.raw_literal "polyIdent"])]) =
        match ResultUtils.getNodeByType children "polyIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:empty_poly", "polyIdent")))
        | ((Some (node))) -> convert_polyIdent node in
      H.Exp.variant ~loc ident.txt None
  | "attribute" ->
      let loc = _loc in
      let ((attr)[@node (("attribute")[@reason.raw_literal "attribute"])]) =
        match ResultUtils.getNodeByType children "attribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:attribute", "attribute")))
        | ((Some (node))) -> convert_attribute node in
      H.Exp.fun_ ~loc "" None
        (H.Pat.var (Location.mkloc (("x")[@reason.raw_literal "x"]) loc))
        (H.Exp.field
           (H.Exp.ident
              (Location.mkloc
                 ((Lident ((("x")[@reason.raw_literal "x"])))) loc)) attr)
  | "op" ->
      let loc = _loc in
      let (((op,oloc))[@text (("operator")[@reason.raw_literal "operator"])])
        =
        match ResultUtils.getLeafByType children "operator" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "Expression:op", "operator"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      H.Exp.ident ~loc
        (Location.mkloc ((Lident (op))) oloc)
  | "tuple" ->
      let loc = _loc in
      let ((exprs)[@nodes (("Expression")[@reason.raw_literal "Expression"])])
        = ResultUtils.getNodesByType children "Expression" convert_Expression in
      H.Exp.tuple ~loc exprs
  | "array_literal" ->
      let loc = _loc in
      let ((items)[@nodes.items
                    (("Expression")[@reason.raw_literal "Expression"])])
        = ResultUtils.getNodesByLabel children "items" convert_Expression in
      H.Exp.array ~loc items
  | "list_literal" ->
      let loc = _loc in
      let ((items)[@nodes.items
                    (("Expression")[@reason.raw_literal "Expression"])])
        = ResultUtils.getNodesByLabel children "items" convert_Expression in
      let ((spread)[@node_opt.spread
                     (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "spread" with
        | None  -> None
        | ((Some (((_,sub),children,loc,comments)))) ->
            ((Some ((convert_Expression (sub, children, loc, comments))))
            ) in
      listToConstruct ~loc items spread
        (fun ~loc  -> fun a  -> fun b  -> H.Exp.construct ~loc a b)
        H.Exp.tuple (fun item  -> item.pexp_loc)
  | "object_literal" ->
      let loc = _loc in
      let ((spread)[@node_opt
                     (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  -> None
        | ((Some (node))) ->
            ((Some ((convert_Expression node)))) in
      let ((items)[@nodes (("ObjectItem")[@reason.raw_literal "ObjectItem"])])
        = ResultUtils.getNodesByType children "ObjectItem" convert_ObjectItem in
      H.Exp.record items spread
  | "let" ->
      let loc = _loc in
      let ((bindings)[@node (("LetPairs")[@reason.raw_literal "LetPairs"])])
        =
        match ResultUtils.getNodeByType children "LetPairs" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:let", "LetPairs")))
        | ((Some (node))) -> convert_LetPairs node in
      let ((body)[@node
                   (("ExpressionSequence")[@reason.raw_literal
                                            "ExpressionSequence"])])
        =
        match ResultUtils.getNodeByType children "ExpressionSequence" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:let", "ExpressionSequence")))
        | ((Some (node))) ->
            convert_ExpressionSequence node in
      H.Exp.let_ ~loc Nonrecursive bindings body
  | "do" ->
      let loc = _loc in
      let ((body)[@node
                   (("ExpressionSequence")[@reason.raw_literal
                                            "ExpressionSequence"])])
        =
        match ResultUtils.getNodeByType children "ExpressionSequence" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:do", "ExpressionSequence")))
        | ((Some (node))) ->
            convert_ExpressionSequence node in
      body
  | "assert" ->
      let loc = _loc in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:assert", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      H.Exp.assert_ ~loc expr
  | "lazy" ->
      let loc = _loc in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:lazy", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      H.Exp.lazy_ ~loc expr
  | "open" ->
      let loc = _loc in
      let ((ident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:open", "longCap")))
        | ((Some (node))) -> convert_longCap node in
      let ((body)[@node
                   (("ExpressionSequence")[@reason.raw_literal
                                            "ExpressionSequence"])])
        =
        match ResultUtils.getNodeByType children "ExpressionSequence" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:open", "ExpressionSequence")))
        | ((Some (node))) ->
            convert_ExpressionSequence node in
      H.Exp.open_ ~loc Fresh ident body
  | "if" ->
      let loc = _loc in
      let ((test)[@node.test
                   (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "test" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:if", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let ((yes)[@node.yes
                  (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "yes" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:if", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let ((no)[@node_opt.no
                 (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "no" with
        | None  -> None
        | ((Some (((_,sub),children,loc,comments)))) ->
            ((Some ((convert_Expression (sub, children, loc, comments))))
            ) in
      H.Exp.ifthenelse ~loc test yes no
  | "module_pack" ->
      let loc = _loc in
      let ((modexp)[@node (("ModuleExpr")[@reason.raw_literal "ModuleExpr"])])
        =
        match ResultUtils.getNodeByType children "ModuleExpr" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:module_pack", "ModuleExpr")))
        | ((Some (node))) -> convert_ModuleExpr node in
      H.Exp.pack ~loc modexp
  | "module" ->
      let loc = _loc in
      let (((text,tloc))[@text
                          (("capIdent")[@reason.raw_literal "capIdent"])])
        =
        match ResultUtils.getLeafByType children "capIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:module", "capIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((modexp)[@node (("ModuleExpr")[@reason.raw_literal "ModuleExpr"])])
        =
        match ResultUtils.getNodeByType children "ModuleExpr" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:module", "ModuleExpr")))
        | ((Some (node))) -> convert_ModuleExpr node in
      let ((body)[@node
                   (("ExpressionSequence")[@reason.raw_literal
                                            "ExpressionSequence"])])
        =
        match ResultUtils.getNodeByType children "ExpressionSequence" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:module", "ExpressionSequence")))
        | ((Some (node))) ->
            convert_ExpressionSequence node in
      H.Exp.letmodule ~loc (Location.mkloc text tloc) modexp body
  | "arrow" ->
      let loc = _loc in
      let ((args)[@node (("FnArgs")[@reason.raw_literal "FnArgs"])]) =
        match ResultUtils.getNodeByType children "FnArgs" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:arrow", "FnArgs")))
        | ((Some (node))) -> convert_FnArgs node in
      let ((body)[@node
                   (("ExpressionSequence")[@reason.raw_literal
                                            "ExpressionSequence"])])
        =
        match ResultUtils.getNodeByType children "ExpressionSequence" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:arrow", "ExpressionSequence")))
        | ((Some (node))) ->
            convert_ExpressionSequence node in
      makeArrow ~args ~body
  | "threading_last" ->
      let loc = _loc in
      let ((target)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:threading_last", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      let ((items)[@nodes (("ThreadItem")[@reason.raw_literal "ThreadItem"])])
        = ResultUtils.getNodesByType children "ThreadItem" convert_ThreadItem in
      Belt.List.reduce items target
        (fun target  ->
           fun (loc,item)  ->
             match item with
             | `Attribute attr -> H.Exp.field ~loc target attr
             | `Fn (fn,args) -> H.Exp.apply fn (args @ [("", target)])
             | `Construct (name,args) ->
                 H.Exp.construct name
                   ((Some ((H.Exp.tuple (args @ [target]))))))
  | "threading" ->
      let loc = _loc in
      let ((target)[@node.target
                     (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "target" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:threading", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let ((items)[@nodes (("ThreadItem")[@reason.raw_literal "ThreadItem"])])
        = ResultUtils.getNodesByType children "ThreadItem" convert_ThreadItem in
      Belt.List.reduce items target
        (fun target  ->
           fun (loc,item)  ->
             match item with
             | `Attribute attr -> H.Exp.field ~loc target attr
             | `Fn (fn,args) -> H.Exp.apply fn (("", target) :: args)
             | `Construct (name,args) ->
                 H.Exp.construct name
                   ((Some ((H.Exp.tuple (target :: args))))))
  | "threading_as" ->
      let loc = _loc in
      let ((target)[@node.target
                     (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "target" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:threading_as", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let ((pat)[@node (("Pattern")[@reason.raw_literal "Pattern"])]) =
        match ResultUtils.getNodeByType children "Pattern" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:threading_as", "Pattern")))
        | ((Some (node))) -> convert_Pattern node in
      let ((items)[@nodes.items
                    (("Expression")[@reason.raw_literal "Expression"])])
        = ResultUtils.getNodesByLabel children "items" convert_Expression in
      Belt.List.reduce items target
        (fun target  ->
           fun item  ->
             H.Exp.apply (H.Exp.fun_ "" None pat item) [("", target)])
  | "switch" ->
      let loc = _loc in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:switch", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      let ((cases)[@nodes (("SwitchCase")[@reason.raw_literal "SwitchCase"])])
        = ResultUtils.getNodesByType children "SwitchCase" convert_SwitchCase in
      H.Exp.match_ ~loc expr cases
  | "switch_function" ->
      let loc = _loc in
      let ((cases)[@nodes (("SwitchCase")[@reason.raw_literal "SwitchCase"])])
        = ResultUtils.getNodesByType children "SwitchCase" convert_SwitchCase in
      H.Exp.function_ ~loc cases
  | "try" ->
      let loc = _loc in
      let ((target)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:try", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      let ((cases)[@nodes (("SwitchCase")[@reason.raw_literal "SwitchCase"])])
        = ResultUtils.getNodesByType children "SwitchCase" convert_SwitchCase in
      H.Exp.try_ ~loc target cases
  | "array_index" ->
      let loc = _loc in
      let ((index)[@node.index
                    (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "index" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:array_index", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let ((array)[@node.array
                    (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "array" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:array_index", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      H.Exp.apply ~loc
        (H.Exp.ident ~loc
           (Location.mkloc
              ((Ldot
                  (((Lident ((("Array")[@reason.raw_literal "Array"])))
                    ),
                    (("get")[@reason.raw_literal "get"]))))
              loc)) [("", array); ("", index)]
  | "js_object_attribute" ->
      let loc = _loc in
      let (((attr,aloc))[@text (("string")[@reason.raw_literal "string"])]) =
        match ResultUtils.getLeafByType children "string" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:js_object_attribute", "string"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((object_)[@node
                      (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:js_object_attribute", "Expression"))
              )
        | ((Some (node))) -> convert_Expression node in
      H.Exp.apply ~loc
        (H.Exp.ident ~loc
           (Location.mkloc
              ((Lident ((("##")[@reason.raw_literal "##"])))) loc))
        [("", object_);
        ("",
          (H.Exp.ident ~loc:aloc
             (Location.mkloc
                ((Lident ((processString attr)))) aloc)))]
  | "setField" ->
      let loc = _loc in
      let ((attribute)[@node
                        (("attribute")[@reason.raw_literal "attribute"])])
        =
        match ResultUtils.getNodeByType children "attribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:setField", "attribute")))
        | ((Some (node))) -> convert_attribute node in
      let ((target)[@node.target
                     (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "target" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:setField", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      let ((value)[@node.value
                    (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByLabel children "value" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:setField", "Expression")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Expression (sub, children, loc, comments) in
      H.Exp.setfield ~loc target attribute value
  | "record_attribute" ->
      let loc = _loc in
      let ((attr)[@node (("attribute")[@reason.raw_literal "attribute"])]) =
        match ResultUtils.getNodeByType children "attribute" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:record_attribute", "attribute"))
              )
        | ((Some (node))) -> convert_attribute node in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:record_attribute", "Expression"))
              )
        | ((Some (node))) -> convert_Expression node in
      H.Exp.field ~loc expr attr
  | "fn_call" ->
      let loc = _loc in
      let ((fn)[@node (("Expression")[@reason.raw_literal "Expression"])]) =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:fn_call", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      let ((args)[@nodes (("FnCallArg")[@reason.raw_literal "FnCallArg"])]) =
        ResultUtils.getNodesByType children "FnCallArg" convert_FnCallArg in
      (match args = [] with
       | true  ->
           H.Exp.apply ~loc fn
             [("",
                (H.Exp.construct ~loc
                   (Location.mkloc
                      ((Lident ((("()")[@reason.raw_literal "()"])))) loc)
                   None))]
       | false  -> H.Exp.apply ~loc fn args)
  | "constraint" ->
      let loc = _loc in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:constraint", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      let ((t)[@node (("CoreType")[@reason.raw_literal "CoreType"])]) =
        match ResultUtils.getNodeByType children "CoreType" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Expression:constraint", "CoreType")))
        | ((Some (node))) -> convert_CoreType node in
      H.Exp.constraint_ ~loc expr t
  | _ -> assert false
and convert_Structure (sub,children,_loc,_comments) =
  match sub with
  | "open" ->
      let loc = _loc in
      let ((lident)[@node (("longCap")[@reason.raw_literal "longCap"])]) =
        match ResultUtils.getNodeByType children "longCap" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "Structure:open", "longCap"))
              )
        | ((Some (node))) -> convert_longCap node in
      H.Str.open_ ~loc (H.Opn.mk lident)
  | "def" ->
      let loc = _loc in
      let ((pair)[@node (("LetPair")[@reason.raw_literal "LetPair"])]) =
        match ResultUtils.getNodeByType children "LetPair" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "Structure:def", "LetPair"))
              )
        | ((Some (node))) -> convert_LetPair node in
      H.Str.value ~loc Nonrecursive [pair]
  | "defn" ->
      let loc = _loc in
      let (((text,tloc))[@text
                          (("lowerIdent")[@reason.raw_literal "lowerIdent"])])
        =
        match ResultUtils.getLeafByType children "lowerIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:defn", "lowerIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((args)[@node (("FnArgs")[@reason.raw_literal "FnArgs"])]) =
        match ResultUtils.getNodeByType children "FnArgs" with
        | None  ->
            raise
              ((PackTypes.ConversionError (_loc, "Structure:defn", "FnArgs"))
              )
        | ((Some (node))) -> convert_FnArgs node in
      let ((body)[@node
                   (("ExpressionSequence")[@reason.raw_literal
                                            "ExpressionSequence"])])
        =
        match ResultUtils.getNodeByType children "ExpressionSequence" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:defn", "ExpressionSequence")))
        | ((Some (node))) ->
            convert_ExpressionSequence node in
      H.Str.value ~loc Nonrecursive
        [H.Vb.mk ~loc (H.Pat.var (Location.mkloc text tloc))
           (makeArrow ~args ~body)]
  | "def_rec" ->
      let loc = _loc in
      let ((pairs)[@nodes (("LetPair")[@reason.raw_literal "LetPair"])]) =
        ResultUtils.getNodesByType children "LetPair" convert_LetPair in
      H.Str.value ~loc Recursive pairs
  | "type" ->
      let loc = _loc in
      let ((pairs)[@nodes (("TypePair")[@reason.raw_literal "TypePair"])]) =
        ResultUtils.getNodesByType children "TypePair" convert_TypePair in
      H.Str.type_ pairs
  | "module" ->
      let loc = _loc in
      let (((name,nameLoc))[@text
                             (("capIdent")[@reason.raw_literal "capIdent"])])
        =
        match ResultUtils.getLeafByType children "capIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:module", "capIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((items)[@nodes (("Structure")[@reason.raw_literal "Structure"])])
        = ResultUtils.getNodesByType children "Structure" convert_Structure in
      H.Str.module_ ~loc
        (H.Mb.mk (Location.mkloc name nameLoc)
           (H.Mod.mk ~loc ((Pmod_structure (items)))))
  | "module_alias" ->
      let loc = _loc in
      let (((name,nameLoc))[@text
                             (("capIdent")[@reason.raw_literal "capIdent"])])
        =
        match ResultUtils.getLeafByType children "capIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:module_alias", "capIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((expr)[@node (("ModuleApply")[@reason.raw_literal "ModuleApply"])])
        =
        match ResultUtils.getNodeByType children "ModuleApply" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:module_alias", "ModuleApply")))
        | ((Some (node))) -> convert_ModuleApply node in
      H.Str.module_ ~loc (H.Mb.mk (Location.mkloc name nameLoc) expr)
  | "external" ->
      let loc = _loc in
      let (((text,tloc))[@text
                          (("lowerIdent")[@reason.raw_literal "lowerIdent"])])
        =
        match ResultUtils.getLeafByType children "lowerIdent" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:external", "lowerIdent")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((typ)[@node (("CoreType")[@reason.raw_literal "CoreType"])]) =
        match ResultUtils.getNodeByType children "CoreType" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:external", "CoreType")))
        | ((Some (node))) -> convert_CoreType node in
      let ((prim)[@texts (("string")[@reason.raw_literal "string"])]) =
        ResultUtils.getLeafsByType children "string" in
      H.Str.primitive ~loc
        (H.Val.mk ~loc
           ~prim:((List.map fst prim) |> (List.map processString))
           (Location.mkloc text tloc) typ)
  | "decorator_nopayload" ->
      let loc = _loc in
      let (((text,loc))[@text
                         (("decoratorName")[@reason.raw_literal
                                             "decoratorName"])])
        =
        match ResultUtils.getLeafByType children "decoratorName" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:decorator_nopayload", "decoratorName"))
              )
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((inner)[@node.inner
                    (("Structure")[@reason.raw_literal "Structure"])])
        =
        match ResultUtils.getNodeByLabel children "inner" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:decorator_nopayload", "Structure"))
              )
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Structure (sub, children, loc, comments) in
      let attr = ((Location.mkloc text loc), ((PStr ([])))) in
      {
        inner with
        pstr_desc =
          ((match inner.pstr_desc with
            | ((Pstr_primitive (vdesc))) ->
                ((Pstr_primitive
                    ({
                       vdesc with
                       pval_attributes = (attr :: (vdesc.pval_attributes))
                     })))
            | ((Pstr_eval (expr,attrs))) ->
                ((Pstr_eval (expr, (attr :: attrs))))
            | _ ->
                failwith
                  (("Decorators only supported for expressions and `external`s")
                  [@reason.raw_literal
                    "Decorators only supported for expressions and `external`s"])))
      }
  | "decorator" ->
      let loc = _loc in
      let (((text,loc))[@text
                         (("decoratorName")[@reason.raw_literal
                                             "decoratorName"])])
        =
        match ResultUtils.getLeafByType children "decoratorName" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:decorator", "decoratorName")))
        | ((Some ((contents,loc)))) -> (contents, loc) in
      let ((payload)[@node.payload
                      (("Structure")[@reason.raw_literal "Structure"])])
        =
        match ResultUtils.getNodeByLabel children "payload" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:decorator", "Structure")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Structure (sub, children, loc, comments) in
      let ((inner)[@node.inner
                    (("Structure")[@reason.raw_literal "Structure"])])
        =
        match ResultUtils.getNodeByLabel children "inner" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:decorator", "Structure")))
        | ((Some (((_,sub),children,loc,comments)))) ->
            convert_Structure (sub, children, loc, comments) in
      let attr =
        ((Location.mkloc text loc), ((PStr ([payload])))) in
      {
        inner with
        pstr_desc =
          ((match inner.pstr_desc with
            | ((Pstr_primitive (vdesc))) ->
                ((Pstr_primitive
                    ({
                       vdesc with
                       pval_attributes = (attr :: (vdesc.pval_attributes))
                     })))
            | ((Pstr_eval (expr,attrs))) ->
                ((Pstr_eval (expr, (attr :: attrs))))
            | _ ->
                failwith
                  (("Decorators only supported for expressions and `external`s")
                  [@reason.raw_literal
                    "Decorators only supported for expressions and `external`s"])))
      }
  | "eval" ->
      let loc = _loc in
      let ((expr)[@node (("Expression")[@reason.raw_literal "Expression"])])
        =
        match ResultUtils.getNodeByType children "Expression" with
        | None  ->
            raise
              ((PackTypes.ConversionError
                  (_loc, "Structure:eval", "Expression")))
        | ((Some (node))) -> convert_Expression node in
      H.Str.eval ~loc expr
  | _ -> assert false
and convert_ExpressionSequence (sub,children,_loc,_comments) =
  let ((exprs)[@nodes (("Expression")[@reason.raw_literal "Expression"])]) =
    ResultUtils.getNodesByType children "Expression" convert_Expression in
  expressionSequence exprs
and convert_ModuleBody (sub,children,_loc,_comments) =
  let ((s)[@nodes (("Structure")[@reason.raw_literal "Structure"])]) =
    ResultUtils.getNodesByType children "Structure" convert_Structure in
  s
and convert_Start (sub,children,_loc,_comments) =
  let ((body)[@node (("ModuleBody")[@reason.raw_literal "ModuleBody"])]) =
    match ResultUtils.getNodeByType children "ModuleBody" with
    | None  ->
        raise
          ((PackTypes.ConversionError (_loc, "Start", "ModuleBody")))
    | ((Some (node))) -> convert_ModuleBody node in
  body
let grammar =
  let open PackTypes.Parsing in
    {
      lineComment =
        ((Some (((";")[@reason.raw_literal ";"]))));
      blockComment =
        ((Some
            (((("(**")[@reason.raw_literal "(**"]),
               (("*)")[@reason.raw_literal "*)"])))));
      rules =
        [("opChar",
           {
             capturesComments = false;
             passThrough = false;
             preserveInnerSpace = false;
             docs = None;
             ignoreNewlines = Inherit;
             leaf = false;
             choices =
               [("", "", (Grammar.choice {|"!"|}));
               ("", "", (Grammar.choice {|"$"|}));
               ("", "", (Grammar.choice {|"%"|}));
               ("", "", (Grammar.choice {|"&"|}));
               ("", "", (Grammar.choice {|"*"|}));
               ("", "", (Grammar.choice {|"+"|}));
               ("", "", (Grammar.choice {|"-"|}));
               ("", "", (Grammar.choice {|"."|}));
               ("", "", (Grammar.choice {|"/"|}));
               ("", "", (Grammar.choice {|"<"|}));
               ("", "", (Grammar.choice {|"="|}));
               ("", "", (Grammar.choice {|">"|}));
               ("", "", (Grammar.choice {|"?"|}));
               ("", "", (Grammar.choice {|"@"|}));
               ("", "", (Grammar.choice {|"^"|}));
               ("", "", (Grammar.choice {|"|" ~"]"|}));
               ("", "", (Grammar.choice {|"~"|}))]
           });
        ("reservedOps",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "", (Grammar.choice {|"=>"|}));
              ("", "", (Grammar.choice {|"->"|}));
              ("", "", (Grammar.choice {|"->>"|}));
              ("", "", (Grammar.choice {|"..."|}))]
          });
        ("operator",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = ((Some (" An operator ")));
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("", " An operator ",
                 (Grammar.choice {|~reservedOps opChar+ ~identchar|}))]
          });
        ("digit",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices = [("", "", (Grammar.choice {|'0..9'|}))]
          });
        ("alpha",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "", (Grammar.choice {|'a..z'|}));
              ("", "", (Grammar.choice {|'A..Z'|}))]
          });
        ("reserved",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "", (Grammar.choice {|"fun"|}));
              ("", "", (Grammar.choice {|"let"|}));
              ("", "", (Grammar.choice {|"and"|}));
              ("", "", (Grammar.choice {|"as"|}));
              ("", "", (Grammar.choice {|"type"|}));
              ("", "", (Grammar.choice {|"switch"|}));
              ("", "", (Grammar.choice {|"exception"|}));
              ("", "", (Grammar.choice {|"external"|}));
              ("", "", (Grammar.choice {|"of"|}));
              ("", "", (Grammar.choice {|"module"|}));
              ("", "", (Grammar.choice {|"rec"|}));
              ("", "", (Grammar.choice {|"open"|}));
              ("", "", (Grammar.choice {|"import"|}));
              ("", "", (Grammar.choice {|"try"|}));
              ("", "", (Grammar.choice {|"catch"|}));
              ("", "", (Grammar.choice {|"from"|}))]
          });
        ("charchar",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "", (Grammar.choice {|"\\" any|}));
              ("", "", (Grammar.choice {|~"'" ~"\n" ~"\\" any|}))]
          });
        ("char",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = ((Some (" A char constant ")));
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("", " A char constant ",
                 (Grammar.choice {|"'" charchar "'"|}))]
          });
        ("longString",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs =
              ((Some
                  (" Note: This doesn't yet support arbitrary heredoc delimiters. Just {| and |} "))
              );
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("",
                 " Note: This doesn't yet support arbitrary heredoc delimiters. Just {| and |} ",
                 (Grammar.choice {a|"{|" (~"|}" any)* "|}"|a}))]
          });
        ("strchar",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "", (Grammar.choice {|"\\" any|}));
              ("", "", (Grammar.choice {|~"\"" ~"\n" ~"\\" any|}))]
          });
        ("string",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = ((Some (" A string constant ")));
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("", " A string constant ",
                 (Grammar.choice {|"\"" strchar* "\""|}))]
          });
        ("float",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = ((Some (" A float constant ")));
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("", " A float constant ",
                 (Grammar.choice {|digit+ '.' digit+|}))]
          });
        ("int64",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = ((Some (" An int constant ")));
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("", " An int constant ",
                 (Grammar.choice {|digit+ ~identchar|}))]
          });
        ("ConstString",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice (("string")[@reason.raw_literal "string"])))]
          });
        ("identchar",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice (("alpha")[@reason.raw_literal "alpha"])));
              ("", "",
                (Grammar.choice (("digit")[@reason.raw_literal "digit"])));
              ("", "", (Grammar.choice {|"_"|}))]
          });
        ("lowerIdent",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs =
              ((Some
                  (" A simple identifier starting with a lower-case letter "))
              );
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("",
                 " A simple identifier starting with a lower-case letter ",
                 (Grammar.choice {|~(reserved ~identchar) 'a..z' identchar*|}))]
          });
        ("capIdent",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs =
              ((Some (" A simple identifier starting with a capital letter "))
              );
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("", " A simple identifier starting with a capital letter ",
                 (Grammar.choice {|~(reserved ~identchar) 'A..Z' identchar*|}))]
          });
        ("polyIdent",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("'`' capIdent")[@reason.raw_literal "'`' capIdent"])))]
          });
        ("constant",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("float", "", (Grammar.choice {|[val]float|}));
              ("int", "", (Grammar.choice {|[val]int64|}));
              ("string", "", (Grammar.choice {|ConstString|}));
              ("longString", "", (Grammar.choice {|longString|}));
              ("char", "", (Grammar.choice {|[val]char|}))]
          });
        ("decoratorChar",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("identchar")[@reason.raw_literal "identchar"])));
              ("", "", (Grammar.choice {|"."|}));
              ("", "", (Grammar.choice {|"+"|}));
              ("", "", (Grammar.choice {|"~"|}))]
          });
        ("decoratorName",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("", "",
                 (Grammar.choice
                    (("decoratorChar+")[@reason.raw_literal "decoratorChar+"])))]
          });
        ("longCap_",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("dot", "", (Grammar.choice {|longCap_ $&"."&$ capIdent|}));
              ("lident", "", (Grammar.choice {|capIdent|}))]
          });
        ("longCap",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs =
              ((Some (" A potentially-namespaced capital identifier "))
              );
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", " A potentially-namespaced capital identifier ",
                 (Grammar.choice {|longCap_ ~"."|}))]
          });
        ("longIdent",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs =
              ((Some (" A potentially-namespaced lower-case identifier "))
              );
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", " A potentially-namespaced lower-case identifier ",
                 (Grammar.choice {|(longCap_ $&"."&$)? lowerIdent|}))]
          });
        ("shortAttribute",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices = [("", "", (Grammar.choice {|':'&$ lowerIdent|}))]
          });
        ("attribute",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices = [("", "", (Grammar.choice {|':'&$ longIdent|}))]
          });
        ("Parened",
          {
            capturesComments = false;
            passThrough = true;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices = [("", "", (Grammar.choice {|"("& Expression & ")"|}))]
          });
        ("argLabel",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("'~' lowerIdent")[@reason.raw_literal "'~' lowerIdent"])))]
          });
        ("argLabelWithConstraint",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("argLabel (':' CoreType)?")[@reason.raw_literal
                                                   "argLabel (':' CoreType)?"])))]
          });
        ("PatternObjectItem",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("normal", "", (Grammar.choice {|attribute Pattern|}));
              ("punned", "", (Grammar.choice {|attribute|}))]
          });
        ("FnArg",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("destructured", "",
                 (Grammar.choice {|argLabelWithConstraint "as" Pattern|}));
              ("optional", "", (Grammar.choice {|argLabel &"=?"|}));
              ("defaulted", "",
                (Grammar.choice {|argLabelWithConstraint &"="& Expression|}));
              ("labeled", "", (Grammar.choice {|argLabelWithConstraint|}));
              ("unlabeled", "", (Grammar.choice {|Pattern|}))]
          });
        ("FnArgItems",
          {
            capturesComments = false;
            passThrough = true;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice (("FnArg+")[@reason.raw_literal "FnArg+"])))]
          });
        ("FnArgs",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("single", "", (Grammar.choice {|lowerIdent|}));
              ("unit", "", (Grammar.choice {|"()"|}));
              ("ignored", "", (Grammar.choice {|"_"|}));
              ("multiple", "", (Grammar.choice {|"["& FnArgItems &"]"|}))]
          });
        ("ObjectItem",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("normal", "", (Grammar.choice {|attribute > Expression|}));
              ("punned", "", (Grammar.choice {|attribute|}))]
          });
        ("ThreadItem",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("attribute", "", (Grammar.choice {|attribute|}));
              ("ident", "", (Grammar.choice {|longIdent|}));
              ("emptyconstr", "", (Grammar.choice {|longCap|}));
              ("constructor", "",
                (Grammar.choice {|"("& longCap Expression+ &")"|}));
              ("fn_call", "",
                (Grammar.choice {|"("& [fn]Expression [args]FnCallArg+ &")"|}))]
          });
        ("SwitchCond",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "", (Grammar.choice {|Pattern ("when" Expression)?|}))]
          });
        ("SwitchCase",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("SwitchCond > Expression")[@reason.raw_literal
                                                  "SwitchCond > Expression"])))]
          });
        ("SwitchBody",
          {
            capturesComments = false;
            passThrough = true;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("SwitchCase+")[@reason.raw_literal "SwitchCase+"])))]
          });
        ("FnCallArg",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("labeled", "",
                 (Grammar.choice {|argLabel $&"="& > Expression|}));
              ("punned", "", (Grammar.choice {|argLabel|}));
              ("expr", "", (Grammar.choice {|Expression|}))]
          });
        ("ValueBinding",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("Pattern > Expression")[@reason.raw_literal
                                               "Pattern > Expression"])))]
          });
        ("typeVariable",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = ((Some (" A type variable ")));
            ignoreNewlines = Inherit;
            leaf = true;
            choices =
              [("", " A type variable ",
                 (Grammar.choice {|'\'' lowerIdent|}))]
          });
        ("CoreType",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("constr_no_args", "", (Grammar.choice {|longIdent|}));
              ("variable", "", (Grammar.choice {|typeVariable|}));
              ("constructor", "",
                (Grammar.choice {|"("& longIdent > CoreType+ &")"|}));
              ("arrow", "",
                (Grammar.choice
                   {|"("& "=>"$ "["& [args]CoreType+ &"]" > CoreType &")"|}))]
          });
        ("TypeConstructor",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("no_args", "", (Grammar.choice {|capIdent|}));
              ("args", "",
                (Grammar.choice {|"("& capIdent > CoreType+ &")"|}))]
          });
        ("shortAttribute",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices = [("", "", (Grammar.choice {|":"$ lowerIdent|}))]
          });
        ("TypeObjectItem",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("normal", "",
                 (Grammar.choice
                    (("shortAttribute > CoreType")[@reason.raw_literal
                                                    "shortAttribute > CoreType"])));
              ("punned", "",
                (Grammar.choice
                   (("shortAttribute")[@reason.raw_literal "shortAttribute"])))]
          });
        ("TypeKind",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("record", "",
                 (Grammar.choice {|"{"& >> TypeObjectItem+ &"}"|}));
              ("constructors", "", (Grammar.choice {|TypeConstructor+|}));
              ("alias", "", (Grammar.choice {|CoreType|}))]
          });
        ("TypeName",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("vbl", "",
                 (Grammar.choice {|"("& lowerIdent > typeVariable+ &")"|}));
              ("plain", "", (Grammar.choice {|lowerIdent|}))]
          });
        ("TypePair",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices = [("", "", (Grammar.choice {|TypeName > TypeKind|}))]
          });
        ("LetPair",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices = [("", "", (Grammar.choice {|Pattern > Expression|}))]
          });
        ("ModuleExpr",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("structure", "",
                 (Grammar.choice {|"("& "str" > Structure* &")"|}));
              ("ident", "", (Grammar.choice {|longCap|}))]
          });
        ("ModuleApply",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices = [("ident", "", (Grammar.choice {|longCap|}))]
          });
        ("TypeBody",
          {
            capturesComments = false;
            passThrough = true;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("TypePair+")[@reason.raw_literal "TypePair+"])))]
          });
        ("Pattern",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("ident", "", (Grammar.choice {|lowerIdent|}));
              ("interval", "",
                (Grammar.choice {|[f]constant &".."& [s]constant|}));
              ("constant", "", (Grammar.choice {|constant|}));
              ("unit", "", (Grammar.choice {|"()"|}));
              ("ignored", "", (Grammar.choice {|"_"|}));
              ("array", "",
                (Grammar.choice
                   {|"["& >> [items]Pattern* ("..."& [spread]Pattern)? &"]"|}));
              ("tuple", "",
                (Grammar.choice {|"("& "," >> Pattern Pattern+ &")"|}));
              ("empty_constr", "", (Grammar.choice {|longCap|}));
              ("poly", "",
                (Grammar.choice {|"("& polyIdent > Pattern+ &")"|}));
              ("empty_poly", "", (Grammar.choice {|polyIdent|}));
              ("exception", "",
                (Grammar.choice {|"("& "exception" > Pattern &")"|}));
              ("constructor", "",
                (Grammar.choice {|"("& longCap > Pattern+ &")"|}));
              ("object", "",
                (Grammar.choice {|"{"& >> PatternObjectItem+ &"}"|}));
              ("or", "", (Grammar.choice {|"(|" >> Pattern+ ")"|}))]
          });
        ("LetPairs",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("", "", (Grammar.choice {| "["& >> ValueBinding+ &"]" |}))]
          });
        ("Expression",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = true;
            docs = None;
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("ident", "", (Grammar.choice {|longIdent|}));
              ("const", "", (Grammar.choice {|constant|}));
              ("unit", "", (Grammar.choice {|"()"|}));
              ("extension_expr", "",
                (Grammar.choice {|"("& "%"& decoratorName Structure+ &")"|}));
              ("decorator_expr_nopayload", "",
                (Grammar.choice
                   {|"("& "@"& decoratorName [inner]Expression &")"|}));
              ("decorator_expr", "",
                (Grammar.choice
                   {|"("& "@"& decoratorName [payload]Structure [inner]Expression &")"|}));
              ("constructor", "",
                (Grammar.choice {|"("& longCap Expression+ &")"|}));
              ("empty_constr", "", (Grammar.choice {|longCap|}));
              ("constructor_poly", "",
                (Grammar.choice {|"("& polyIdent > Expression+ &")"|}));
              ("empty_poly", "", (Grammar.choice {|polyIdent|}));
              ("attribute", "", (Grammar.choice {|attribute|}));
              ("op", "", (Grammar.choice {|operator|}));
              ("tuple", "",
                (Grammar.choice {|"("& "," >> Expression Expression+ &")"|}));
              ("array_literal", "",
                (Grammar.choice {|"[|"& >> [items]Expression* &"|]"|}));
              ("list_literal", "",
                (Grammar.choice
                   {|"["& >> [items]Expression* ("..."& [spread]Expression)? &"]"|}));
              ("object_literal", "",
                (Grammar.choice
                   {|"{"& >> ("..."& Expression)? ObjectItem* &"}"|}));
              ("let", "",
                (Grammar.choice
                   {|"("& "let"$ LetPairs > ExpressionSequence &")"|}));
              ("do", "",
                (Grammar.choice {|"("& "do" > ExpressionSequence &")"|}));
              ("assert", "",
                (Grammar.choice {|"("& "assert" > Expression &")"|}));
              ("lazy", "",
                (Grammar.choice {|"("& "lazy" > Expression &")"|}));
              ("open", "",
                (Grammar.choice
                   {|"("& "open"$ longCap > ExpressionSequence &")"|}));
              ("if", "",
                (Grammar.choice
                   {|"("& "if"$ [test]Expression > [yes]Expression [no]Expression? &")"|}));
              ("module_pack", "",
                (Grammar.choice {|"("& "module"$ ModuleExpr &")"|}));
              ("module", "",
                (Grammar.choice
                   {|"("& "module"$ capIdent$ ModuleExpr > ExpressionSequence &")"|}));
              ("arrow", "",
                (Grammar.choice
                   {|"("& "=>"$ FnArgs > ExpressionSequence &")"|}));
              ("threading_last", "",
                (Grammar.choice {|"("& "->>"$ Expression > ThreadItem* &")"|}));
              ("threading", "",
                (Grammar.choice
                   {|"("& "->"$ [target]Expression > ThreadItem* &")"|}));
              ("threading_as", "",
                (Grammar.choice
                   {|"("& "as->"$ [target]Expression$ Pattern > [items]Expression* &")"|}));
              ("switch", "",
                (Grammar.choice
                   {|"("& "switch"$ Expression > SwitchBody &")"|}));
              ("switch_function", "",
                (Grammar.choice {|"("& "switch"$ "_" > SwitchBody &")"|}));
              ("try", "",
                (Grammar.choice
                   {|"("& "try"$ [target]Expression > SwitchCase+ &")"|}));
              ("array_index", "",
                (Grammar.choice
                   {|"("& "["& [index]Expression &"]" > [array]Expression &")"|}));
              ("js_object_attribute", "",
                (Grammar.choice {|"("& string > Expression &")"|}));
              ("setField", "",
                (Grammar.choice
                   {|"("& "<-"$ attribute > [target]Expression [value]Expression &")"|}));
              ("record_attribute", "",
                (Grammar.choice {|"("& attribute > Expression &")"|}));
              ("fn_call", "",
                (Grammar.choice {|"("& Expression > FnCallArg* &")"|}));
              ("constraint", "",
                (Grammar.choice {|"("& ":"$ Expression > CoreType &")"|}))]
          });
        ("Structure",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = true;
            docs =
              ((Some
                  (" Forms that are valid at the top level of a file or module "))
              );
            ignoreNewlines = Inherit;
            leaf = false;
            choices =
              [("open", "", (Grammar.choice {|"("& "open" > longCap &")"|}));
              ("def", " Define a toplevel value. ",
                (Grammar.choice {|"("& "def"$ LetPair &")"|}));
              ("defn", "",
                (Grammar.choice
                   {|"("& "defn"$ lowerIdent$ FnArgs > ExpressionSequence &")"|}));
              ("def_rec", "",
                (Grammar.choice {|"("& "def-rec"$ LetPair+ &")"|}));
              ("type", "", (Grammar.choice {|"("& "type"$ TypeBody &")"|}));
              ("module", "",
                (Grammar.choice {|"("& "module"$ capIdent > Structure+ &")"|}));
              ("module_alias", "",
                (Grammar.choice
                   {|"("& "module-alias"$ capIdent > ModuleApply &")"|}));
              ("external", "",
                (Grammar.choice
                   {|"("& "external" lowerIdent CoreType string+ &")"|}));
              ("decorator_nopayload", "",
                (Grammar.choice
                   {|"("& "@"& decoratorName [inner]Structure &")"|}));
              ("decorator", "",
                (Grammar.choice
                   {|"("& "@"& decoratorName [payload]Structure [inner]Structure &")"|}));
              ("eval", "",
                (Grammar.choice
                   (("Expression")[@reason.raw_literal "Expression"])))]
          });
        ("ExpressionSequence",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = true;
            docs = None;
            ignoreNewlines = Yes;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("Expression*")[@reason.raw_literal "Expression*"])))]
          });
        ("ModuleBody",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = true;
            docs = None;
            ignoreNewlines = Yes;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("Structure+")[@reason.raw_literal "Structure+"])))]
          });
        ("Start",
          {
            capturesComments = false;
            passThrough = false;
            preserveInnerSpace = false;
            docs = None;
            ignoreNewlines = Yes;
            leaf = false;
            choices =
              [("", "",
                 (Grammar.choice
                    (("ModuleBody")[@reason.raw_literal "ModuleBody"])))]
          })]
    }
let start ~filename  text =
  match Runtime.parse ~filename grammar
          (("Start")[@reason.raw_literal "Start"]) text
  with
  | ((Belt.Result.Error
      ((((Some
        (((Node
         (((("Start")[@reason.raw_literal "Start"]),sub),children,loc,comments))
         )))),e))))
      ->
      ((Belt.Result.Error
          ((((Some ((convert_Start (sub, children, loc, comments))))), e)))
      )
  | ((Belt.Result.Error ((_,e)))) ->
      ((Belt.Result.Error ((None, e))))
  | ((Ok
      (((Node
       (((("Start")[@reason.raw_literal "Start"]),sub),children,loc,comments))
       ))))
      ->
      ((Ok ((convert_Start (sub, children, loc, comments)))))
  | Ok _ ->
      failwith (("Invalid response")[@reason.raw_literal "Invalid response"])
end
module Monads
= struct
#1 "Monads.ml"
[@@@ocaml.ppx.context { cookies = [] }]
type ('a,'b) either =
  | Left of 'a
  | Right of 'b
let leftForce a =
  match a with
  | ((Left (a))) -> a
  | Right _ ->
      failwith (("Expected a left")[@reason.raw_literal "Expected a left"])
let rightForce a =
  match a with
  | ((Right (a))) -> a
  | Left _ ->
      failwith (("Expected a right")[@reason.raw_literal "Expected a right"])
module type MonadThing  =
  sig
    type 'a t
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val consume : 'a t -> f:('a -> unit) -> unit
    val join2 : 'a t -> 'b t -> ('a* 'b) t
  end
module Continuation =
  struct
    type 'a t = ('a -> unit) -> unit
    let return x fin = fin x
    let map work ~f:use  fin = work (fun result  -> fin (use result))
    let bind work ~f:use  fin = work (fun result  -> (use result) fin)
    let consume work ~f:use  = work use
    type ('a,'b) side =
      | One of 'a
      | Two of 'b
      | Neither
      | Done
    let join2 one two fin =
      let side = ref Neither in
      one
        (fun one  ->
           match !side with
           | Neither  -> side := ((One (one)))
           | ((Two (two))) ->
               (side := Done; fin (one, two))
           | One _|Done  -> ());
      two
        (fun two  ->
           match !side with
           | Neither  -> side := ((Two (two)))
           | ((One (one))) ->
               (side := Done; fin (one, two))
           | Two _|Done  -> ())
    let first one two fin =
      let finished = ref false in
      one
        (fun one  ->
           if not (!finished)
           then (finished := true; fin ((Left (one)))));
      two
        (fun two  ->
           if not (!finished)
           then (finished := true; fin ((Right (two)))))
  end
module C : MonadThing = Continuation 
module NodeContinuation =
  struct
    open Belt.Result
    type ('a,'b) t = (('a,'b) Belt.Result.t -> unit) -> unit
    let return x fin = fin ((Ok (x)))
    let map work ~f:use  fin =
      work (fun result  -> fin ((Ok ((use result)))))
    let bind work ~f:use  fin = work (fun result  -> (use result) fin)
    let consume work ~f:use  = work use
    type ('a,'b) side =
      | One of 'a
      | Two of 'b
      | Neither
      | Done
    let join2 one two fin =
      let side = ref Neither in
      one
        (fun oneRes  ->
           match !side with
           | Neither  ->
               (match oneRes with
                | ((Ok (oneVal))) ->
                    side := ((One (oneVal)))
                | ((Error (err))) ->
                    (side := Done; fin ((Error (err)))))
           | ((Two (twoVal))) ->
               (match oneRes with
                | ((Ok (oneVal))) ->
                    (side := Done;
                     fin ((Ok ((oneVal, twoVal)))))
                | ((Error (err))) ->
                    fin ((Error (err))))
           | One _|Done  -> ());
      two
        (fun two  ->
           match !side with
           | Neither  ->
               (match two with
                | ((Ok (two))) ->
                    side := ((Two (two)))
                | ((Error (err))) ->
                    (side := Done; fin ((Error (err)))))
           | ((One (one))) ->
               (match two with
                | ((Ok (two))) ->
                    (side := Done; fin ((Ok ((one, two)))))
                | ((Error (err))) ->
                    fin ((Error (err))))
           | Two _|Done  -> ())
  end
module Option =
  struct
    type 'a t = 'a option
    let return x = ((Some (x)))
    let map value ~f:use  =
      match value with
      | ((Some (x))) ->
          ((Some ((use x))))
      | None  -> None
    let bind value ~f:use  =
      match value with
      | ((Some (x))) -> use x
      | None  -> None
    let consume value ~f:use  =
      match value with
      | ((Some (x))) -> use x
      | None  -> ()
    let force value ~f:use  =
      match value with
      | ((Some (x))) -> use x
      | None  ->
          failwith
            (("Tried to unwrap an empty optional")[@reason.raw_literal
                                                    "Tried to unwrap an empty optional"])
    let join2 one two =
      match one with
      | None  -> None
      | ((Some (one))) ->
          (match two with
           | None  -> None
           | ((Some (two))) ->
               ((Some ((one, two)))))
    let first one two =
      match one with
      | ((Some (one))) ->
          ((Some (((Left (one))))))
      | None  ->
          (match two with
           | ((Some (two))) ->
               ((Some (((Right (two))))))
           | None  -> None)
  end
module O : MonadThing = Option 
module Result =
  struct
    open Belt.Result
    let return x = ((Ok (x)))
    let map value ~f:use  =
      match value with
      | ((Ok (x))) -> ((Ok ((use x))))
      | ((Error (e))) -> ((Error (e)))
    let bind: ('a,'b) t -> f:('a -> ('c,'b) t) -> ('c,'b) t =
      fun value  ->
        fun ~f:use  ->
          match value with
          | ((Ok (x))) -> use x
          | ((Error (e))) ->
              ((Error (e)))
    let consume: ('a,'b) t -> f:('a -> unit) -> unit =
      fun value  ->
        fun ~f:use  ->
          match value with
          | ((Ok (x))) -> use x
          | Error _ -> ()
    let force: ('a,'b) t -> f:('a -> 'c) -> 'c =
      fun value  ->
        fun ~f:use  ->
          match value with
          | ((Ok (x))) -> use x
          | ((Error (error))) -> failwith error
    let join2 one two =
      match one with
      | ((Error (e))) -> ((Error (e)))
      | ((Ok (v1))) ->
          (match two with
           | ((Error (e))) ->
               ((Error (e)))
           | ((Ok (v2))) ->
               ((Ok ((v1, v2)))))
    let first one two =
      match one with
      | ((Ok (x))) ->
          ((Ok (((Left (x))))))
      | ((Error (_e))) ->
          (match two with
           | ((Ok (x))) ->
               ((Ok (((Right (x))))))
           | ((Error (e))) ->
               ((Error (e))))
    let unwrap value map other_return =
      match value with
      | ((Ok (x))) -> map x ~f:return
      | ((Error (err))) ->
          other_return ((Error (err)))
  end
end
module Pretty
= struct
#1 "Pretty.ml"
[@@@ocaml.ppx.context { cookies = [] }]
[@@@ocaml.doc
  " Taken with modifications from https://github.com/t0yv0/ocaml-pretty/blob/master/pretty.ml "]
type breakMode =
  | Normal
  | CannotFlatten
  | BreakAfter
type doc =
  {
  node: node;
  break_mode: breakMode;
  flat_size: int;
  min_width: int;
  single_line: bool;}
and node =
  | Append of doc* doc
  | Empty
  | Group of doc
  | FullIndent of doc
  | NewLine
  | Indent of int* doc
  | BackLine of int* string
  | Line of int* string
  | Text of int* string
let append left right =
  match ((left.node), (right.node)) with
  | (Empty ,_) -> right
  | (_,Empty ) -> left
  | _ ->
      {
        node = ((Append (left, right)));
        break_mode =
          ((match ((left.break_mode), (right.break_mode)) with
            | (CannotFlatten ,_)|(_,CannotFlatten ) -> CannotFlatten
            | (BreakAfter ,_) -> CannotFlatten
            | (_,BreakAfter ) -> BreakAfter
            | _ -> Normal));
        flat_size = (left.flat_size + right.flat_size);
        min_width =
          (if left.single_line
           then left.min_width + right.min_width
           else left.min_width);
        single_line = (left.single_line && right.single_line)
      }
let empty =
  {
    node = Empty;
    flat_size = 0;
    min_width = 0;
    single_line = true;
    break_mode = Normal
  }
let back num text =
  {
    node = ((BackLine (num, text)));
    flat_size = 0;
    min_width = 0;
    single_line = true;
    break_mode = Normal
  }
let group doc = { doc with node = ((Group (doc))) }
let indent amount doc =
  { doc with node = ((Indent (amount, doc))) }
let fullIndent doc =
  { doc with node = ((FullIndent (doc))) }
let newLine =
  {
    node = NewLine;
    flat_size = 0;
    min_width = 0;
    single_line = false;
    break_mode = Normal
  }
let line defaultString =
  let length = String.length defaultString in
  {
    node = ((Line (length, defaultString)));
    flat_size = length;
    min_width = 0;
    single_line = false;
    break_mode = Normal
  }
let text ?len  string =
  let len =
    match len with
    | None  -> String.length string
    | ((Some (n))) -> n in
  {
    node = ((Text (len, string)));
    flat_size = len;
    min_width = len;
    single_line = true;
    break_mode = Normal
  }
let multiLine string =
  {
    break_mode = CannotFlatten;
    node = ((Text (0, string)));
    single_line = false;
    flat_size = (String.length string);
    min_width = (String.length string)
  }
let breakAfter ?len  string =
  { (text ?len string) with break_mode = BreakAfter }
let dontFlatten doc = { doc with break_mode = CannotFlatten }
let rec flatten doc =
  match doc.node with
  | ((Append (a,b))) -> append (flatten a) (flatten b)
  | Empty |Text _ -> doc
  | NewLine  -> empty
  | ((Group (x)))|((Indent (_,x))) ->
      flatten x
  | ((FullIndent (x))) -> flatten x
  | ((Line (_,x))) -> text x
  | ((BackLine (_,x))) -> text x
type stack_node = {
  doc: doc;
  min_total: int;
  offset: int;}
type stack =
  | Nil
  | Cons of stack_node* stack
let min_total stack =
  match stack with
  | Nil  -> 0
  | ((Cons (head,_))) -> head.min_total
let push offset node (stack : stack) =
  let current_min_total =
    if node.single_line
    then (min_total stack) + node.min_width
    else node.min_width in
  ((Cons ({ doc = node; offset; min_total = current_min_total }, stack))
    )
let break = line ""
let space = line ((" ")[@reason.raw_literal " "])
let dedent = back 2 ""
let str = text
let (@!) = append
let print_indentation n = for i = 1 to n do print_char ' ' done
let prettyString ?(width= 100)  doc print =
  let buffer = Buffer.create 100 in
  print ?width:((Some (width)))
    ?output:((Some ((fun text  -> Buffer.add_string buffer text))))
    ?indent:((Some
                ((fun num  ->
                    for i = 1 to num do Buffer.add_char buffer ' ' done)))
    ) doc;
  (Buffer.to_bytes buffer) |> Bytes.to_string
let print ?(width= 70)  ?(output= print_string)  ?(indent= print_indentation)
   doc =
  let rec loop currentIndent stack =
    match stack with
    | Nil  -> ()
    | ((Cons (stackNode,rest))) ->
        let offset = stackNode.offset in
        (match (stackNode.doc).node with
         | ((Append (left,right))) ->
             loop currentIndent (push offset left (push offset right rest))
         | Empty  -> loop currentIndent rest
         | ((Group (doc))) ->
             let flatDoc =
               if
                 (doc.break_mode <> CannotFlatten) &&
                   ((doc.flat_size + (min_total rest)) <=
                      (width - currentIndent))
               then flatten doc
               else doc in
             loop currentIndent (push offset flatDoc rest)
         | NewLine  ->
             (output (("\n")[@reason.raw_literal "\\n"]);
              loop currentIndent rest)
         | ((FullIndent (doc))) ->
             loop currentIndent (push currentIndent doc rest)
         | ((Indent (ident,doc))) ->
             (indent ident;
              loop (currentIndent + ident) (push (offset + ident) doc rest))
         | ((BackLine (num,_))) ->
             (output (("\n")[@reason.raw_literal "\\n"]);
              indent (offset - num);
              loop (offset - num) rest)
         | Line _ ->
             (output (("\n")[@reason.raw_literal "\\n"]);
              indent offset;
              loop offset rest)
         | ((Text (len,string))) ->
             (output string; loop (currentIndent + len) rest)) in
  loop 0 (push 0 doc Nil)[@@test.call
                           fun doc  ->
                             prettyString ~width:10 (group doc) print]
  [@@test
    [(empty, "");
    ((str (("Hello")[@reason.raw_literal "Hello"])),
      (("Hello")[@reason.raw_literal "Hello"]));
    (((str (("Hello")[@reason.raw_literal "Hello"])) @!
        (break @!
           (str (("Folks_and_Folks")[@reason.raw_literal "Folks_and_Folks"])))),
      (("Hello\nFolks_and_Folks")[@reason.raw_literal
                                   "Hello\\nFolks_and_Folks"]));
    (((str (("Hello")[@reason.raw_literal "Hello"])) @!
        (break @!
           (newLine @!
              (str
                 (("Folks_and_Folks")[@reason.raw_literal "Folks_and_Folks"]))))),
      (("Hello\n\nFolks_and_Folks")[@reason.raw_literal
                                     "Hello\\n\\nFolks_and_Folks"]));
    (((str (("Hello")[@reason.raw_literal "Hello"])) @!
        (break @!
           (indent 4
              (str
                 (("Folks_and_Folks")[@reason.raw_literal "Folks_and_Folks"]))))),
      (("Hello\n    Folks_and_Folks")[@reason.raw_literal
                                       "Hello\\n    Folks_and_Folks"]));
    (((str (("Hello")[@reason.raw_literal "Hello"])) @!
        ((str ((" ")[@reason.raw_literal " "])) @!
           (fullIndent
              ((str (("12345")[@reason.raw_literal "12345"])) @!
                 (break @!
                    (str (("54321234")[@reason.raw_literal "54321234"]))))))),
      (("Hello 12345\n      54321234")[@reason.raw_literal
                                        "Hello 12345\\n      54321234"]))]]
end
module NewPrettyPrint
= struct
#1 "NewPrettyPrint.ml"
[@@@ocaml.ppx.context { cookies = [] }]
open PackTypes.Result
open PackTypes.Parsing
let rec maybeFind children check =
  match children with
  | [] -> (None, [])
  | child::rest ->
      (match check child with
       | None  ->
           let (res,rest) = maybeFind rest check in (res, (child :: rest))
       | x -> (x, rest))
let findByLabel children needle =
  match maybeFind children
          (fun (label,child)  ->
             match label = needle with
             | true  -> ((Some (child)))
             | false  -> None)
  with
  | (None ,c) ->
      (((Belt.Result.Error
           ((("No child found for ")[@reason.raw_literal
                                      "No child found for "])))), c)
  | (((Some (c))),u) -> (((Ok (c))), u)
let findByType children needle =
  match maybeFind children
          (fun (label,child)  ->
             if label = ""
             then
               match child with
               | ((Leaf ((name,sub),_,_)))|((Node
                 ((name,sub),_,_,_))) when name = needle ->
                   ((Some (child)))
               | _ -> None
             else None)
  with
  | (((Some (x))),c) ->
      (((Belt.Result.Ok (x))), c)
  | (None ,c) ->
      (((Error
           (((("Cannot find child for type ")[@reason.raw_literal
                                               "Cannot find child for type "])
               ^ needle)))), c)
let passThroughChildren grammar name =
  let rule =
    try List.assoc name grammar.rules
    with
    | Not_found  ->
        failwith
          ((("Undefined rule name: ")[@reason.raw_literal
                                       "Undefined rule name: "])
             ^ name) in
  if rule.passThrough
  then
    let (a,b,c) = List.hd rule.choices in
    ((Some ((c, (rule.ignoreNewlines)))))
  else None
let break = Pretty.line ""
let space = Pretty.line ((" ")[@reason.raw_literal " "])
let dedent = Pretty.back 2 ""
let str = Pretty.text
let (@!) = Pretty.append
open Belt.Result
let mergeSides ~preserveInnerLine  ar bl a b canSpace aloc bloc =
  match (ar, bl) with
  | (`MustBreak,_)|(_,`MustBreak) -> a @! (break @! b)
  | (`Tight,_)|(_,`Tight) -> a @! b
  | (`Space,_)|(_,`Space) ->
      a @! ((str ((" ")[@reason.raw_literal " "])) @! b)
  | (`Normal,`Normal) when canSpace ->
      if
        preserveInnerLine &&
          (((bloc.Location.loc_start).pos_lnum -
              (aloc.Location.loc_end).pos_lnum)
             > 1)
      then a @! (Pretty.newLine @! (space @! b))
      else a @! (space @! b)
  | _ -> a @! (break @! b)
let combine ?(preserveInnerLine= false)  item res canSpace =
  match (item, res) with
  | (one,`Empty) -> one
  | (`Empty,one) -> one
  | (`Sides (al,ar,a,aloc),`Sides (bl,br,b,bloc)) ->
      `Sides
        (al, br,
          (mergeSides ~preserveInnerLine ar bl a b canSpace aloc bloc),
          ((match aloc = Location.none with
            | true  -> bloc
            | false  ->
                (match bloc = Location.none with
                 | true  -> aloc
                 | false  -> { aloc with loc_end = (bloc.loc_end) }))))
let unwrap item =
  match item with | `Empty -> Pretty.empty | `Sides (_,_,a,_) -> a
let map fn item =
  match item with
  | `Empty -> `Empty
  | `Sides (l,r,a,loc) -> `Sides (l, r, (fn a), loc)
let mergeOne a b =
  match (a, b) with
  | (`MustBreak,_)|(_,`MustBreak) -> `MustBreak
  | (`Tight,_)|(_,`Tight) -> `Tight
  | (`Space,_)|(_,`Space) -> `Space
  | (`Break,_)|(_,`Break) -> `Break
  | _ -> `Normal
let left item newL =
  match item with
  | `Empty -> `Empty
  | `Sides (l,r,a,loc) -> `Sides ((mergeOne l newL), r, a, loc)
let right item newR =
  match item with
  | `Empty -> `Empty
  | `Sides (l,r,a,loc) -> `Sides (l, (mergeOne r newR), a, loc)
let rec greedy rule isLexical loop p children min max =
  if max = 0
  then ((Ok ((`Empty, children))))
  else
    (match loop [p] children with
     | ((Error (message))) ->
         (match min <= 0 with
          | true  -> ((Ok ((`Empty, children))))
          | false  -> ((Error (message))))
     | ((Ok ((res,unused)))) when children = unused ->
         ((Ok ((res, unused))))
     | ((Ok ((res,unused)))) ->
         (match greedy rule isLexical loop p unused (min - 1) (max - 1) with
          | ((Ok ((r2,u2)))) when r2 = `Empty ->
              ((Ok ((res, u2))))
          | ((Ok ((r2,u2)))) ->
              ((Ok
                  (((combine ~preserveInnerLine:(rule.preserveInnerSpace) res
                       r2 (not isLexical)), u2))))
          | ((Error (message))) ->
              (match min <= 1 with
               | true  -> ((Ok ((res, unused))))
               | false  -> ((Error (message))))))
let prependItem sep item k =
  ((Monads.Result.bind k
      ~f:(fun (res,unused)  ->
            ((Ok (((combine item res sep), unused))))))
  [@ocaml.explanation "Sugar for the Result type"])
let rec singleOutput rule grammar ignoringNewlines isLexical item children
  loop =
  match item with
  | ((Terminal (text,None ))) ->
      ((Ok
          (((`Sides (`Normal, `Normal, (str text), Location.none)), children)))
      )
  | ((Terminal (text,((Some (label))))))
      ->
      (match findByLabel children label with
       | (((Error (m))),_) ->
           ((Error (m)))
       | (((Ok (x))),children) ->
           ((Ok
               (((`Sides (`Normal, `Normal, (str text), Location.none)),
                  children)))))
  | ((NonTerminal (name,label))) ->
      processNonTerminal grammar name label children ignoringNewlines loop
  | ((NoSpaceAfter (p))) ->
      ((Monads.Result.bind
          (singleOutput rule grammar ignoringNewlines isLexical p children
             loop)
          ~f:(fun (a,b)  -> ((Ok (((right a `Tight), b))))))
      [@ocaml.explanation "Sugar for the Result type"])
  | ((NoSpaceBefore (p))) ->
      ((Monads.Result.bind
          (singleOutput rule grammar ignoringNewlines isLexical p children
             loop)
          ~f:(fun (a,b)  -> ((Ok (((left a `Tight), b))))))
      [@ocaml.explanation "Sugar for the Result type"])
  | ((NoBreakAfter (p))) ->
      ((Monads.Result.bind
          (singleOutput rule grammar ignoringNewlines isLexical p children
             loop)
          ~f:(fun (a,b)  -> ((Ok (((right a `Space), b))))))
      [@ocaml.explanation "Sugar for the Result type"])
  | ((NoBreakBefore (p))) ->
      ((Monads.Result.bind
          (singleOutput rule grammar ignoringNewlines isLexical p children
             loop)
          ~f:(fun (a,b)  -> ((Ok (((left a `Space), b))))))
      [@ocaml.explanation "Sugar for the Result type"])
  | ((Lexify (p))) ->
      singleOutput rule grammar ignoringNewlines isLexical p children loop
  | ((Group (p))) -> loop ignoringNewlines p children
  | CommentEOL  ->
      ((Ok
          (((`Sides (`Normal, `Normal, (Pretty.breakAfter ""), Location.none)),
             children))))
  | EOF |Empty |Lookahead _|Not _|Indent |FullIndent  ->
      ((Ok ((`Empty, children))))
  | ((Star (p))) ->
      greedy rule isLexical (loop ignoringNewlines) p children 0 (-1)
  | ((Plus (p))) ->
      greedy rule isLexical (loop ignoringNewlines) p children 1 (-1)
  | ((Optional (p))) ->
      ((Monads.Result.bind
          (greedy rule isLexical (loop ignoringNewlines) p children 0 1)
          ~f:(fun (res,unused)  ->
                if unused = children
                then ((Ok ((`Empty, unused))))
                else ((Ok ((res, unused))))))[@ocaml.explanation
                                                                 "Sugar for the Result type"])
  | Any _|Chars _ ->
      ((Error
          ((("Chars should be within a @leaf, not at the top level")[@reason.raw_literal
                                                                    "Chars should be within a @leaf, not at the top level"])))
      )
and outputItem rule grammar ~isLexical  ignoringNewlines items children =
  let loop = outputItem rule grammar ~isLexical in
  match children with
  | ("",((Comment (EOL ,contents,cloc))))::rest ->
      (loop ignoringNewlines items rest) |>
        (prependItem false
           (`Sides (`Normal, `MustBreak, (Pretty.breakAfter contents), cloc)))
  | ("",((Comment ((Multi |Doc ),contents,cloc))))::rest ->
      (loop ignoringNewlines items rest) |>
        (prependItem false
           (`Sides (`Normal, `Normal, (Pretty.multiLine contents), cloc)))
  | _ ->
      (match items with
       | [] -> ((Ok ((`Empty, children))))
       | (Indent )::rest ->
           ((Monads.Result.bind (loop ignoringNewlines rest children)
               ~f:(fun (res2,unused)  ->
                     ((Ok
                         (((map (fun m  -> Pretty.indent 2 m) res2), unused)))
                     )))[@ocaml.explanation
                                            "Sugar for the Result type"])
       | (FullIndent )::rest ->
           ((Monads.Result.bind (loop ignoringNewlines rest children)
               ~f:(fun (res2,unused)  ->
                     ((Ok
                         (((map (fun m  -> Pretty.fullIndent m) res2),
                            unused))))))[@ocaml.explanation
                                                            "Sugar for the Result type"])
       | item::[] ->
           singleOutput rule grammar ignoringNewlines isLexical item children
             loop
       | item::rest ->
           ((Monads.Result.bind
               (singleOutput rule grammar ignoringNewlines isLexical item
                  children loop)
               ~f:(fun (res,unused)  ->
                     ((Monads.Result.bind (loop ignoringNewlines rest unused)
                         ~f:(fun (res2,unused)  ->
                               if res = `Empty
                               then ((Ok ((res2, unused))))
                               else
                                 if isLexical
                                 then
                                   ((Ok (((combine res res2 false), unused)))
                                   )
                                 else
                                   ((Ok
                                       (((combine
                                            ~preserveInnerLine:(rule.preserveInnerSpace)
                                            res res2 true), unused)))
                                   )))[@ocaml.explanation
                                                          "Sugar for the Result type"])))
           [@ocaml.explanation "Sugar for the Result type"]))
and processNonTerminal grammar name label children ignoringNewlines loop =
  match passThroughChildren grammar name with
  | ((Some ((subs,ignoreNewlines)))) ->
      let newIgnore =
        match (ignoreNewlines, ignoringNewlines) with
        | (Yes ,_) -> true
        | (No ,_) -> false
        | (Inherit ,x) -> x in
      ((Monads.Result.bind (loop newIgnore subs children)
          ~f:(fun (res,unused)  -> ((Ok ((res, unused))))))
        [@ocaml.explanation "Sugar for the Result type"])
  | None  ->
      let (child,others) =
        match label with
        | ((Some (label))) -> findByLabel children label
        | None  -> findByType children name in
      ((Monads.Result.bind child
          ~f:(fun result  ->
                ((Monads.Result.bind
                    (resultToPretty ignoringNewlines grammar result)
                    ~f:(fun output  ->
                          ((Ok
                              (((`Sides
                                   (`Normal, `Normal, (Pretty.group output),
                                     (PackTypes.Result.loc result))), others)))
                          )))[@ocaml.explanation
                                                 "Sugar for the Result type"])))
        [@ocaml.explanation "Sugar for the Result type"])
and resultToPretty:
  bool -> grammar -> result -> (Pretty.doc,string) Belt.Result.t =
  fun ignoringNewlines  ->
    fun grammar  ->
      fun result  ->
        match result with
        | ((Leaf (_,contents,_))) ->
            ((Ok ((Pretty.text contents))))
        | ((Comment (EOL ,contents,_))) ->
            ((Ok ((Pretty.breakAfter contents))))
        | ((Comment (Doc ,contents,_))) ->
            ((Ok ((Pretty.text contents))))
        | ((Comment (Multi ,contents,_))) ->
            ((Ok ((Pretty.text contents))))
        | ((Node ((name,sub),children,_,_comments))) ->
            ((Monads.Result.bind
                (nodeToPretty ignoringNewlines grammar (name, sub) children)
                ~f:(fun res  -> ((Ok (res)))))[@ocaml.explanation
                                                                  "Sugar for the Result type"])
and nodeToPretty ignoringNewlines grammar (ruleName,sub) children =
  let rule = List.assoc ruleName grammar.rules in
  ((Monads.Result.bind
      (match List.find (fun (name,_,_)  -> name = sub) rule.choices with
       | exception Not_found  ->
           ((Error
               (((("No rule sub ")[@reason.raw_literal "No rule sub "]) ^
                   (ruleName ^ (((" : ")[@reason.raw_literal " : "]) ^ sub)))))
           )
       | x -> ((Ok (x))))
      ~f:(fun (_,_,items)  ->
            let ignoringNewlines =
              match ((rule.ignoreNewlines), ignoringNewlines) with
              | (Yes ,_) -> true
              | (No ,_) -> false
              | (Inherit ,x) -> x in
            let isLexical = (Char.uppercase (ruleName.[0])) <> (ruleName.[0]) in
            ((Monads.Result.bind
                (outputItem rule grammar ~isLexical ignoringNewlines items
                   children)
                ~f:(fun (result,unused)  ->
                      match unused with
                      | [] -> ((Ok ((unwrap result))))
                      | _ ->
                          ((Error
                              (((("Failed to print ")[@reason.raw_literal
                                                       "Failed to print "])
                                  ^
                                  (ruleName ^
                                     (((" : ")[@reason.raw_literal " : "]) ^
                                        sub))))))))
              [@ocaml.explanation "Sugar for the Result type"])))[@ocaml.explanation
                                                                   "Sugar for the Result type"])
let prettyString ?(width= 100)  doc =
  let buffer = Buffer.create 100 in
  Pretty.print ~width ~output:(fun text  -> Buffer.add_string buffer text)
    ~indent:(fun num  -> for i = 1 to num do Buffer.add_char buffer ' ' done)
    doc;
  (Buffer.to_bytes buffer) |> Bytes.to_string
let toPretty (grammar : grammar) result = resultToPretty false grammar result
let startToString ?(maxWidth= 30)  grammar (sub,children,loc,comments) =
  let node =
    ((Node
        (((("Start")[@reason.raw_literal "Start"]), sub), children, loc,
          comments))) in
  ((Monads.Result.bind (resultToPretty false grammar node)
      ~f:(fun pretty  ->
            ((Ok ((prettyString ~width:maxWidth pretty))))))
    [@ocaml.explanation "Sugar for the Result type"])
end
module Sysop
= struct
#1 "sysop.ml"
[@@@ocaml.ppx.context { cookies = [] }]
let argv = Sys.argv
let readChan chan =
  let lines = ref [] in
  (try while true do lines := ((input_line chan) :: (!lines)) done
   with | End_of_file  -> ());
  (let lines = List.rev (!lines) in
   String.concat (("\n")[@reason.raw_literal "\\n"]) lines)
let readStdin () = readChan stdin
let readFile x = (open_in x) |> readChan
end
module Lisp
= struct
#1 "lisp.ml"
[@@@ocaml.ppx.context { cookies = [] }]
let getContents input =
  match input with
  | (("-")[@reason.raw_literal "-"]) -> Sysop.readStdin ()
  | x -> Sysop.readFile x
let out_binary (ast : Parsetree.structure) input_name =
  set_binary_mode_out stdout true;
  output_string stdout Config.ast_impl_magic_number;
  output_value stdout input_name;
  output_value stdout ast
let _ = Printexc.record_backtrace true
type printType =
  | Bin
  | Pretty of string
  | Debug
  | Ml
let _ =
  match Sysop.argv with
  | [|_;(("docs")[@reason.raw_literal "docs"])|] ->
      let open PackTypes.Parsing in
        (print_endline
           (("# Syntax for Lisp.re\n")[@reason.raw_literal
                                        "# Syntax for Lisp.re\\n"]);
         print_endline
           (ExampleGenerator.help ^
              (("\n\nIf you're interested, <a href=\"../grammars/lispGrammar.re\">take a look at the grammar definition</a>")
              [@reason.raw_literal
                "\\n\\nIf you're interested, <a href=\\\"../grammars/lispGrammar.re\\\">take a look at the grammar definition</a>"]));
         print_endline (ExampleGenerator.docsForGrammar LispGrammar.grammar);
         exit 0)
  | [|_;(("docs-docs")[@reason.raw_literal "docs-docs"])|] ->
      let open PackTypes.Parsing in
        (print_endline
           (("# Syntax for the Parser Generator\n")[@reason.raw_literal
                                                     "# Syntax for the Parser Generator\\n"]);
         print_endline
           (ExampleGenerator.help ^
              (("\n\nIf you're interested, <a href=\"../parsable/grammar\">take a look at the grammar definition</a>")
              [@reason.raw_literal
                "\\n\\nIf you're interested, <a href=\\\"../parsable/grammar\\\">take a look at the grammar definition</a>"]));
         print_endline
           (ExampleGenerator.docsForGrammar GrammarGrammar.grammar);
         exit 0)
  | [|_;(("--print")[@reason.raw_literal "--print"]);(("re")[@reason.raw_literal
                                                              "re"]);width;(("--parse")
      [@reason.raw_literal "--parse"]);(("re")[@reason.raw_literal "re"])|]
      ->
      (match Str.split (Str.regexp_string (("=")[@reason.raw_literal "="]))
               width
       with
       | _::width::[] ->
           let width = int_of_string width in
           let raw = Sysop.readStdin () in
           let result =
             Grammar.getResult LispGrammar.grammar
               (("Start")[@reason.raw_literal "Start"]) raw in
           ((match NewPrettyPrint.startToString LispGrammar.grammar result
             with
             | ((Ok (res))) -> print_string res
             | ((Error (message))) ->
                 (Printf.eprintf
                    (("Unable to print %s")[@reason.raw_literal
                                             "Unable to print %s"]) message;
                  exit 1));
            exit 0)
       | _ ->
           (Printf.eprintf
              (("Invalid width declaration")[@reason.raw_literal
                                              "Invalid width declaration"]);
            exit 1))
  | [|_;(("--print")[@reason.raw_literal "--print"]);(("binary")[@reason.raw_literal
                                                                  "binary"]);(("--parse")
      [@reason.raw_literal "--parse"]);(("re")[@reason.raw_literal "re"])|]
      ->
      let raw = Sysop.readStdin () in
      let result =
        match LispGrammar.start (("Start")[@reason.raw_literal "Start"]) raw
        with
        | x -> x
        | exception ((PackTypes.ConversionError
            (loc,ruleName,searchedFor))) ->
            failwith
              ((("Grammar error! Please report this to the lisp.re maintainers. Unable to find ")
                 [@reason.raw_literal
                   "Grammar error! Please report this to the lisp.re maintainers. Unable to find "])
                 ^
                 (searchedFor ^
                    (((" in ")[@reason.raw_literal " in "]) ^
                       (ruleName ^
                          (((" at ")[@reason.raw_literal " at "]) ^
                             (PackTypes.Result.showLoc loc)))))) in
      ((match result with
        | ((Ok (converted))) ->
            out_binary converted
              (("inputfile.rel")[@reason.raw_literal "inputfile.rel"])
        | ((Error ((_,(_,loc,failure))))) ->
            (Printf.eprintf
               (("File \"%s\", line %d, characters %d-%d:\n%s")[@reason.raw_literal
                                                                 "File \\\"%s\\\", line %d, characters %d-%d:\\n%s"])
               loc.pos_fname loc.pos_lnum (loc.pos_cnum - loc.pos_bol)
               ((loc.pos_cnum - loc.pos_bol) + 10)
               (PackTypes.Error.errorsText (snd failure));
             exit 1));
       exit 0)
  | [|_;(("--print")[@reason.raw_literal "--print"]);(("binary")[@reason.raw_literal
                                                                  "binary"]);(("--recoverable")
      [@reason.raw_literal "--recoverable"]);(("--parse")[@reason.raw_literal
                                                           "--parse"]);(("re")
      [@reason.raw_literal "re"])|] ->
      let raw = Sysop.readStdin () in
      let result =
        match LispGrammar.start (("Start")[@reason.raw_literal "Start"]) raw
        with
        | x -> x
        | exception ((PackTypes.ConversionError
            (loc,ruleName,searchedFor))) ->
            failwith
              ((("Grammar error! Please report this to the lisp.re maintainers. Unable to find ")
                 [@reason.raw_literal
                   "Grammar error! Please report this to the lisp.re maintainers. Unable to find "])
                 ^
                 (searchedFor ^
                    (((" in ")[@reason.raw_literal " in "]) ^
                       (ruleName ^
                          (((" at ")[@reason.raw_literal " at "]) ^
                             (PackTypes.Result.showLoc loc)))))) in
      ((match result with
        | ((Ok (converted))) ->
            out_binary converted
              (("inputfile.rel")[@reason.raw_literal "inputfile.rel"])
        | ((Error
            ((((Some (converted))),(_,loc,failure))))
            ) ->
            (out_binary converted
               (("inputfile.rel")[@reason.raw_literal "inputfile.rel"]);
             Printf.eprintf
               (("File \"%s\", line %d, characters %d-%d:\n%s")[@reason.raw_literal
                                                                 "File \\\"%s\\\", line %d, characters %d-%d:\\n%s"])
               loc.pos_fname loc.pos_lnum (loc.pos_cnum - loc.pos_bol)
               ((loc.pos_cnum - loc.pos_bol) + 10)
               (PackTypes.Error.errorsText (snd failure)))
        | ((Error ((None ,(_,loc,failure))))) ->
            (out_binary []
               (("inputfile.rel")[@reason.raw_literal "inputfile.rel"]);
             Printf.eprintf
               (("File \"%s\", line %d, characters %d-%d:\n%s")[@reason.raw_literal
                                                                 "File \\\"%s\\\", line %d, characters %d-%d:\\n%s"])
               loc.pos_fname loc.pos_lnum (loc.pos_cnum - loc.pos_bol)
               ((loc.pos_cnum - loc.pos_bol) + 10)
               (PackTypes.Error.errorsText (snd failure))));
       exit 0)
  | _ -> ()
let (command,input) =
  match Sysop.argv with
  | [|_;(("pretty")[@reason.raw_literal "pretty"]);input;output|] ->
      (((Pretty (output))), input)
  | [|_;command;input|] ->
      (((match command with
         | (("bin")[@reason.raw_literal "bin"]) -> Bin
         | (("ml")[@reason.raw_literal "ml"]) -> Ml
         | (("debug")[@reason.raw_literal "debug"]) -> Debug
         | (("pretty")[@reason.raw_literal "pretty"]) ->
             ((Pretty ((("-")[@reason.raw_literal "-"]))))
         | _ ->
             failwith
               (("Invalid command")[@reason.raw_literal "Invalid command"]))),
        input)
  | [|_;input|] -> (Debug, input)
  | _ ->
      (print_endline
         {|Usage: [command=debug] inputfile

Commands:
  bin    - parse & print the binary representation of the ocaml ast to stdout
  ml     - parse & print as ocaml syntax
  debug  - parse & print the ocaml AST as human-readable text
  pretty - parse & pretty print as lisp
|};
       exit 1)
let result =
  Grammar.getResult LispGrammar.grammar
    (("Start")[@reason.raw_literal "Start"]) (getContents input)
let converted = LispGrammar.convert_Start result
let _ =
  match command with
  | Debug  -> Printast.implementation Format.std_formatter converted
  | Ml  -> Pprintast.structure Format.std_formatter converted
  | Bin  -> out_binary converted input
  | ((Pretty (dest))) ->
      (match NewPrettyPrint.startToString LispGrammar.grammar result with
       | ((Belt.Result.Ok (x))) ->
           (match dest with
            | (("-")[@reason.raw_literal "-"]) -> print_endline x
            | _ -> output_string (open_out dest) x)
       | ((Error (message))) ->
           failwith
             ((("Failed to pretty print :( ")[@reason.raw_literal
                                               "Failed to pretty print :( "])
                ^ message))
end
module LispToOcaml
= struct
#1 "lispToOcaml.ml"
[@@@ocaml.ppx.context { cookies = [] }]
open Parsetree
open PackTypes.Result
open Longident
open Location
open Lexing
open Asttypes
open Infix
module H = Ast_helper
module RU = ResultUtils
let unwrap = ResultUtils.unwrap
let unwrapWith message opt =
  match opt with
  | None  -> failwith message
  | ((Some (x))) -> x
let loc = !H.default_loc
let str = H.Str.eval (H.Exp.array [])
type loc = PackTypes.Result.loc
type fromOcaml =
  {
  fromStructure: fromOcaml -> structure_item -> result;
  fromExpression: fromOcaml -> expression -> result;}
type toOcaml =
  {
  toLoc: loc -> Location.t;
  expression:
    toOcaml ->
      (string* (string* result) list* loc* PackTypes.Result.comments option)
        -> expression;
  structure:
    toOcaml ->
      (string* (string* result) list* loc* PackTypes.Result.comments option)
        -> structure_item;}
let sliceToEnd s start =
  let l = String.length s in
  match start <= l with
  | true  -> String.sub s start (l - start)
  | false  -> ""
let stripQuotes str = String.sub str 1 ((String.length str) - 2)
let processString str = (str |> stripQuotes) |> Scanf.unescaped
let failexpr message =
  H.Exp.apply
    (H.Exp.ident
       (Location.mknoloc
          ((Longident.Lident ((("failwith")[@reason.raw_literal "failwith"])))
          )))
    [("",
       (Ast_helper.Exp.constant
          ((Const_string (message, None)))))]
let stripRuleName ((name,sub),children,loc,comments) =
  (sub, children, loc, comments)
let rec parseLongCap_ (sub,children,_,_) =
  match sub with
  | (("lident")[@reason.raw_literal "lident"]) ->
      ((Lident
          (((RU.getContentsByType children
               (("capIdent")[@reason.raw_literal "capIdent"]))
              |> unwrap))))
  | (("dot")[@reason.raw_literal "dot"]) ->
      ((Ldot
          ((((RU.getNodeByType children
                (("longCap_")[@reason.raw_literal "longCap_"]))
               |> unwrap)
              |> parseLongCap_),
            ((RU.getContentsByType children
                (("capIdent")[@reason.raw_literal "capIdent"]))
               |> unwrap))))
  | _ ->
      failwith
        ((("Invalid longCap_ sub ")[@reason.raw_literal
                                     "Invalid longCap_ sub "])
           ^ sub)
let parseLongCap (_,children,_,_) =
  ((RU.getNodeByType children (("longCap_")[@reason.raw_literal "longCap_"]))
     |> unwrap)
    |> parseLongCap_
let parseLongIdent (_,children,_,_) =
  let first =
    (RU.getNodeByType children (("longCap_")[@reason.raw_literal "longCap_"]))
      |?>> parseLongCap_ in
  let last =
    (RU.getContentsByType children
       (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
      |> unwrap in
  match first with
  | ((Some (x))) -> ((Ldot (x, last)))
  | None  -> ((Lident (last)))
let parseCoreType toOcaml (sub,children,loc,_) =
  match sub with
  | (("constr_no_args")[@reason.raw_literal "constr_no_args"]) ->
      let name =
        ((RU.getNodeByType children
            (("longIdent")[@reason.raw_literal "longIdent"]))
           |> unwrap)
          |> parseLongIdent in
      H.Typ.constr (Location.mkloc name (toOcaml.toLoc loc)) []
  | _ ->
      failwith
        ((("unhandled core type sub ")[@reason.raw_literal
                                        "unhandled core type sub "])
           ^ sub)
let parseTypeKind toOcaml (sub,children,loc,_) =
  match sub with
  | (("record")[@reason.raw_literal "record"]) ->
      ((Ptype_record
          ((RU.getNodesByType children
              (("TypeObjectItem")[@reason.raw_literal "TypeObjectItem"])
              (fun (sub,children,loc,_)  ->
                 let (name,nameLoc) =
                   let (_,children,loc,_) =
                     (RU.getNodeByType children
                        (("shortAttribute")[@reason.raw_literal
                                             "shortAttribute"]))
                       |> unwrap in
                   (((RU.getContentsByType children
                        (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
                       |> unwrap), loc) in
                 let t =
                   match sub = (("punned")[@reason.raw_literal "punned"])
                   with
                   | true  ->
                       H.Typ.constr
                         (Location.mkloc ((Lident (name)))
                            (toOcaml.toLoc nameLoc)) []
                   | false  ->
                       parseCoreType toOcaml
                         ((RU.getNodeByType children
                             (("CoreType")[@reason.raw_literal "CoreType"]))
                            |> unwrap) in
                 let name = Location.mkloc name (toOcaml.toLoc loc) in
                 H.Type.field name t)))))
  | _ ->
      failwith
        ((("Unsupported type kind ")[@reason.raw_literal
                                      "Unsupported type kind "])
           ^ sub)
let rec parsePattern toOcaml (sub,children,loc,_) =
  let ocamlLoc = toOcaml.toLoc loc in
  match sub with
  | (("ident")[@reason.raw_literal "ident"]) ->
      H.Pat.var ~loc:ocamlLoc
        (Location.mkloc
           ((RU.getContentsByType children
               (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
              |> unwrap) (toOcaml.toLoc loc))
  | (("object")[@reason.raw_literal "object"]) ->
      H.Pat.record ~loc:ocamlLoc
        (RU.getNodesByType children
           (("PatternObjectItem")[@reason.raw_literal "PatternObjectItem"])
           (fun (sub,children,loc,_)  ->
              let (name,nameLoc) =
                let (_,children,loc,_) =
                  (RU.getNodeByType children
                     (("attribute")[@reason.raw_literal "attribute"]))
                    |>
                    (unwrapWith
                       (("No attribute")[@reason.raw_literal "No attribute"])) in
                ((((RU.getNodeByType children
                      (("longIdent")[@reason.raw_literal "longIdent"]))
                     |>
                     (unwrapWith
                        (("No longident")[@reason.raw_literal "No longident"])))
                    |> parseLongIdent), loc) in
              let t =
                match sub = (("punned")[@reason.raw_literal "punned"]) with
                | true  ->
                    H.Pat.var
                      (Location.mkloc (Longident.last name)
                         (toOcaml.toLoc nameLoc))
                | false  ->
                    parsePattern toOcaml
                      ((RU.getNodeByType children
                          (("Pattern")[@reason.raw_literal "Pattern"]))
                         |>
                         (unwrapWith
                            (("No pat")[@reason.raw_literal "No pat"]))) in
              ((Location.mkloc name (toOcaml.toLoc nameLoc)), t))) Open
  | _ ->
      failwith
        ((("Not supportdd pattern sub: ")[@reason.raw_literal
                                           "Not supportdd pattern sub: "])
           ^ sub)
let parseConstant (sub,children,loc,_) =
  let raw =
    (RU.getContentsByLabel children (("val")[@reason.raw_literal "val"])) |>
      unwrap in
  match sub with
  | (("string")[@reason.raw_literal "string"]) ->
      ((Const_string ((processString raw), None)))
  | (("int")[@reason.raw_literal "int"]) ->
      ((Const_int ((int_of_string raw))))
  | (("float")[@reason.raw_literal "float"]) ->
      ((Const_float (raw)))
  | _ ->
      failwith
        (("Unhandled constant type")[@reason.raw_literal
                                      "Unhandled constant type"])
let rec listToConstruct list maybeRest typeC tupleC =
  match list with
  | [] ->
      (match maybeRest with
       | None  ->
           typeC
             (Location.mkloc
                ((Lident ((("[]")[@reason.raw_literal "[]"])))) loc) None
       | ((Some (x))) -> x)
  | one::rest ->
      typeC
        (Location.mkloc
           ((Lident ((("::")[@reason.raw_literal "::"]))))
           loc)
        ((Some ((tupleC [one; listToConstruct rest maybeRest typeC tupleC])))
        )
let parseArrow toOcaml (sub,children,loc,_) =
  let res =
    ((RU.getNodeByType children
        (("Expression")[@reason.raw_literal "Expression"]))
       |> (unwrapWith (("no expr")[@reason.raw_literal "no expr"])))
      |> (toOcaml.expression toOcaml) in
  let (sub,children,loc,_) =
    (RU.getNodeByType children (("FnArgs")[@reason.raw_literal "FnArgs"])) |>
      (unwrapWith (("no args")[@reason.raw_literal "no args"])) in
  match sub with
  | (("single")[@reason.raw_literal "single"]) ->
      H.Exp.fun_ "" None
        (H.Pat.var
           (Location.mknoloc
              ((RU.getContentsByType children
                  (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
                 |>
                 (unwrapWith (("no ident")[@reason.raw_literal "no ident"])))))
        res
  | (("unit")[@reason.raw_literal "unit"]) ->
      H.Exp.fun_ "" None
        (H.Pat.construct
           (Location.mknoloc
              ((Lident ((("()")[@reason.raw_literal "()"]))))) None) res
  | (("ignored")[@reason.raw_literal "ignored"]) ->
      H.Exp.fun_ "" None (H.Pat.any ()) res
  | (("multiple")[@reason.raw_literal "multiple"]) ->
      let rec loop items res =
        match items with
        | [] -> res
        | (sub,children,loc,_)::rest ->
            let (pattern,label) =
              match sub with
              | (("unlabeled")[@reason.raw_literal "unlabeled"]) ->
                  ((parsePattern toOcaml
                      ((RU.getNodeByType children
                          (("Pattern")[@reason.raw_literal "Pattern"]))
                         |>
                         (unwrapWith
                            (("No pat")[@reason.raw_literal "No pat"])))),
                    None)
              | (("labeled")[@reason.raw_literal "labeled"]) ->
                  let label =
                    (RU.getContentsByType children
                       (("argLabel")[@reason.raw_literal "argLabel"]))
                      |>
                      (unwrapWith
                         (("No label")[@reason.raw_literal "No label"])) in
                  let label = sliceToEnd label 1 in
                  let pattern =
                    H.Pat.var (Location.mkloc label (toOcaml.toLoc loc)) in
                  (pattern, ((Some (label))))
              | _ ->
                  failwith
                    ((("Unhandled pattern kind ")[@reason.raw_literal
                                                   "Unhandled pattern kind "])
                       ^ sub) in
            loop rest (H.Exp.fun_ (label |? "") None pattern res) in
      loop
        (RU.getNodesByType children (("FnArg")[@reason.raw_literal "FnArg"])
           (fun x  -> x)) res
  | _ ->
      failwith
        (("Can't parse this arrow")[@reason.raw_literal
                                     "Can't parse this arrow"])
let parseExpression toOcaml (sub,children,loc,comments) =
  let oloc = toOcaml.toLoc loc in
  match sub with
  | (("array_index")[@reason.raw_literal "array_index"]) ->
      failexpr
        (("Array index not done")[@reason.raw_literal "Array index not done"])
  | (("fn_call")[@reason.raw_literal "fn_call"]) ->
      H.Exp.apply ~loc:oloc
        (toOcaml.expression toOcaml
           (((RU.getNodeByLabel children (("fn")[@reason.raw_literal "fn"]))
               |> unwrap)
              |> stripRuleName))
        (RU.getNodesByType children
           (("FnCallArg")[@reason.raw_literal "FnCallArg"])
           (fun (sub,children,loc,_)  ->
              match sub with
              | (("expr")[@reason.raw_literal "expr"]) ->
                  ("",
                    (toOcaml.expression toOcaml
                       ((RU.getNodeByType children
                           (("Expression")[@reason.raw_literal "Expression"]))
                          |> unwrap)))
              | (("labeled")[@reason.raw_literal "labeled"]) ->
                  let name =
                    (RU.getContentsByType children
                       (("argLabel")[@reason.raw_literal "argLabel"]))
                      |>
                      (unwrapWith
                         (("no arg label")[@reason.raw_literal
                                            "no arg label"])) in
                  let name = sliceToEnd name 1 in
                  let expr =
                    toOcaml.expression toOcaml
                      ((RU.getNodeByType children
                          (("Expression")[@reason.raw_literal "Expression"]))
                         |>
                         (unwrapWith
                            (("No expr")[@reason.raw_literal "No expr"]))) in
                  (name, expr)
              | (("punned")[@reason.raw_literal "punned"]) ->
                  let name =
                    (RU.getContentsByType children
                       (("argLabel")[@reason.raw_literal "argLabel"]))
                      |>
                      (unwrapWith (("No pun")[@reason.raw_literal "No pun"])) in
                  let name = sliceToEnd name 1 in
                  (name,
                    (H.Exp.ident
                       (Location.mkloc ((Lident (name)))
                          (toOcaml.toLoc loc))))
              | _ ->
                  failwith
                    ((("Unvalid fncallarg sub ")[@reason.raw_literal
                                                  "Unvalid fncallarg sub "])
                       ^ sub)))
  | (("threading_last")[@reason.raw_literal "threading_last"]) ->
      let target =
        (((RU.getNodeByLabel children
             (("target")[@reason.raw_literal "target"]))
            |> unwrap)
           |> stripRuleName)
          |> (toOcaml.expression toOcaml) in
      let items =
        RU.getNodesByType children
          (("ThreadItem")[@reason.raw_literal "ThreadItem"]) (fun x  -> x) in
      let rec loop target items =
        match items with
        | [] -> target
        | ((("ident")[@reason.raw_literal "ident"]),children,_,_)::rest ->
            loop
              (H.Exp.apply ~loc:oloc
                 (H.Exp.ident
                    (Location.mkloc
                       (parseLongIdent
                          ((RU.getNodeByType children
                              (("longIdent")[@reason.raw_literal "longIdent"]))
                             |> unwrap)) (toOcaml.toLoc loc))) [("", target)])
              rest
        | ((("fn_call")[@reason.raw_literal "fn_call"]),children,_,_)::rest
            ->
            loop
              (H.Exp.apply
                 (toOcaml.expression toOcaml
                    (((RU.getNodeByLabel children
                         (("fn")[@reason.raw_literal "fn"]))
                        |> unwrap)
                       |> stripRuleName))
                 (((RU.getNodesByLabel children
                      (("args")[@reason.raw_literal "args"])
                      (toOcaml.expression toOcaml))
                     |> (List.map (fun m  -> ("", m))))
                    @ [("", target)])) rest
        | _ -> failexpr (("Unable to")[@reason.raw_literal "Unable to"]) in
      loop target items
  | (("attribute")[@reason.raw_literal "attribute"]) ->
      let (name,nameLoc) =
        let (_,children,loc,_) =
          (RU.getNodeByType children
             (("attribute")[@reason.raw_literal "attribute"]))
            |>
            (unwrapWith
               (("No attribute")[@reason.raw_literal "No attribute"])) in
        ((((RU.getNodeByType children
              (("longIdent")[@reason.raw_literal "longIdent"]))
             |>
             (unwrapWith
                (("No longident")[@reason.raw_literal "No longident"])))
            |> parseLongIdent), loc) in
      H.Exp.fun_ ~loc:oloc "" None
        (H.Pat.var (Location.mknoloc (("x")[@reason.raw_literal "x"])))
        (H.Exp.field
           (H.Exp.ident
              (Location.mknoloc
                 ((Lident ((("x")[@reason.raw_literal "x"]))))))
           (Location.mkloc name (toOcaml.toLoc nameLoc)))
  | (("op")[@reason.raw_literal "op"]) ->
      H.Exp.ident ~loc:oloc
        (Location.mkloc
           ((Lident
               (((RU.getContentsByType children
                    (("operator")[@reason.raw_literal "operator"]))
                   |> unwrap)))) (toOcaml.toLoc loc))
  | (("ident")[@reason.raw_literal "ident"]) ->
      H.Exp.ident ~loc:oloc
        (Location.mkloc
           (parseLongIdent
              ((RU.getNodeByType children
                  (("longIdent")[@reason.raw_literal "longIdent"]))
                 |> unwrap)) (toOcaml.toLoc loc))
  | (("arrow")[@reason.raw_literal "arrow"]) ->
      parseArrow toOcaml
        ((RU.getNodeByType children (("Arrow")[@reason.raw_literal "Arrow"]))
           |> unwrap)
  | (("record_attribute")[@reason.raw_literal "record_attribute"]) ->
      let (name,nameLoc) =
        let (_,children,loc,_) =
          (RU.getNodeByType children
             (("attribute")[@reason.raw_literal "attribute"]))
            |>
            (unwrapWith
               (("No attribute")[@reason.raw_literal "No attribute"])) in
        ((((RU.getNodeByType children
              (("longIdent")[@reason.raw_literal "longIdent"]))
             |>
             (unwrapWith
                (("No longident")[@reason.raw_literal "No longident"])))
            |> parseLongIdent), loc) in
      H.Exp.field ~loc:oloc
        (((RU.getNodeByType children
             (("Expression")[@reason.raw_literal "Expression"]))
            |> unwrap)
           |> (toOcaml.expression toOcaml))
        (Location.mkloc name (toOcaml.toLoc nameLoc))
  | (("const")[@reason.raw_literal "const"]) ->
      H.Exp.constant ~loc:oloc
        (parseConstant
           ((RU.getNodeByType children
               (("constant")[@reason.raw_literal "constant"]))
              |> unwrap))
  | (("array_literal")[@reason.raw_literal "array_literal"]) ->
      listToConstruct
        (RU.getNodesByLabel children (("items")[@reason.raw_literal "items"])
           (toOcaml.expression toOcaml))
        (((RU.getNodeByLabel children
             (("spread")[@reason.raw_literal "spread"]))
            |?>> stripRuleName)
           |?>> (toOcaml.expression toOcaml)) H.Exp.construct H.Exp.tuple
  | (("object_literal")[@reason.raw_literal "object_literal"]) ->
      H.Exp.record ~loc:oloc
        (RU.getNodesByType children
           (("ObjectItem")[@reason.raw_literal "ObjectItem"])
           (fun (sub,children,loc,_)  ->
              let (name,nameLoc) =
                let (_,children,loc,_) =
                  (RU.getNodeByType children
                     (("attribute")[@reason.raw_literal "attribute"]))
                    |>
                    (unwrapWith
                       (("No attribute")[@reason.raw_literal "No attribute"])) in
                ((((RU.getNodeByType children
                      (("longIdent")[@reason.raw_literal "longIdent"]))
                     |>
                     (unwrapWith
                        (("No longident")[@reason.raw_literal "No longident"])))
                    |> parseLongIdent), loc) in
              let t =
                match sub = (("punned")[@reason.raw_literal "punned"]) with
                | true  ->
                    H.Exp.ident
                      (Location.mkloc
                         ((Lident ((Longident.last name))))
                         (toOcaml.toLoc nameLoc))
                | false  ->
                    toOcaml.expression toOcaml
                      ((RU.getNodeByType children
                          (("Expression")[@reason.raw_literal "Expression"]))
                         |>
                         (unwrapWith
                            (("No expr")[@reason.raw_literal "No expr"]))) in
              ((Location.mkloc name (toOcaml.toLoc nameLoc)), t)))
        (((RU.getNodeByLabel children
             (("spread")[@reason.raw_literal "spread"]))
            |?>> stripRuleName)
           |?>> (toOcaml.expression toOcaml))
  | _ ->
      failexpr
        ((("Unexpected expression type: ")[@reason.raw_literal
                                            "Unexpected expression type: "])
           ^ sub)
let parseLetPair toOcaml (sub,children,loc,_) =
  let pat =
    ((RU.getNodeByType children (("Pattern")[@reason.raw_literal "Pattern"]))
       |> (unwrapWith (("no pattern")[@reason.raw_literal "no pattern"])))
      |> (parsePattern toOcaml) in
  let init =
    ((RU.getNodeByType children
        (("Expression")[@reason.raw_literal "Expression"]))
       |> (unwrapWith (("no expr")[@reason.raw_literal "no expr"])))
      |> (toOcaml.expression toOcaml) in
  H.Vb.mk pat init
let parseStructure toOcaml (sub,children,loc,_) =
  match sub with
  | (("module")[@reason.raw_literal "module"]) ->
      let name =
        (RU.getContentsByType children
           (("capIdent")[@reason.raw_literal "capIdent"]))
          |> unwrap in
      let (sub,children,loc,_) =
        (RU.getNodeByType children
           (("ModuleExpr")[@reason.raw_literal "ModuleExpr"]))
          |> unwrap in
      let desc =
        match sub with
        | (("structure")[@reason.raw_literal "structure"]) ->
            let items =
              RU.getNodesByType children
                (("Structure")[@reason.raw_literal "Structure"])
                (toOcaml.structure toOcaml) in
            ((Pmod_structure (items)))
        | _ ->
            failwith
              (("Unhandled module type")[@reason.raw_literal
                                          "Unhandled module type"]) in
      H.Str.module_
        (H.Mb.mk (Location.mkloc name (toOcaml.toLoc loc)) (H.Mod.mk desc))
  | (("eval")[@reason.raw_literal "eval"]) ->
      H.Str.eval
        (((RU.getNodeByType children
             (("Expression")[@reason.raw_literal "Expression"]))
            |> unwrap)
           |> (toOcaml.expression toOcaml))
  | (("type")[@reason.raw_literal "type"]) ->
      H.Str.type_
        (RU.getNodesByType children
           (("TypePair")[@reason.raw_literal "TypePair"])
           (fun (sub,children,loc,_)  ->
              let (name,vbls) =
                let (sub,children,loc,_) =
                  (RU.getNodeByType children
                     (("TypeName")[@reason.raw_literal "TypeName"]))
                    |> unwrap in
                let name =
                  (RU.getContentsByType children
                     (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
                    |> unwrap in
                match sub with
                | (("plain")[@reason.raw_literal "plain"]) -> (name, [])
                | (("vbl")[@reason.raw_literal "vbl"]) ->
                    (name,
                      (RU.getManyContentsByType children
                         (("typeVariable")[@reason.raw_literal
                                            "typeVariable"])))
                | _ ->
                    failwith
                      (("Invalid typ typ")[@reason.raw_literal
                                            "Invalid typ typ"]) in
              let kind =
                parseTypeKind toOcaml
                  ((RU.getNodeByType children
                      (("TypeDecl")[@reason.raw_literal "TypeDecl"]))
                     |> unwrap) in
              H.Type.mk
                ~params:(vbls |>
                           (List.map
                              (fun name  -> ((H.Typ.var name), Invariant))))
                ~kind (Location.mkloc name (toOcaml.toLoc loc))))
  | (("open")[@reason.raw_literal "open"]) ->
      H.Str.open_
        (H.Opn.mk
           (Location.mkloc
              (parseLongCap
                 ((RU.getNodeByType children
                     (("longCap")[@reason.raw_literal "longCap"]))
                    |> unwrap)) (toOcaml.toLoc loc)))
  | (("let_rec")[@reason.raw_literal "let_rec"]) ->
      H.Str.value Recursive
        (RU.getNodesByType children
           (("LetPair")[@reason.raw_literal "LetPair"])
           (fun pair  -> parseLetPair toOcaml pair))
  | (("let")[@reason.raw_literal "let"]) ->
      H.Str.value Nonrecursive
        [parseLetPair toOcaml
           ((RU.getNodeByType children
               (("LetPair")[@reason.raw_literal "LetPair"]))
              |>
              (unwrapWith
                 (("no let pair")[@reason.raw_literal "no let pair"])))]
  | _ ->
      Ast_helper.Str.eval
        (failexpr
           ((("Unexpected sub: ")[@reason.raw_literal "Unexpected sub: "]) ^
              sub))
let toOcaml =
  {
    toLoc = (fun l  -> Location.none);
    structure = parseStructure;
    expression = parseExpression
  }
let calcBols text =
  let lines =
    Str.split (Str.regexp_string (("\n")[@reason.raw_literal "\\n"])) text in
  let (_,bols) =
    Belt.List.reduce lines (0, [])
      (fun (offset,results)  ->
         fun line  ->
           (((offset + (String.length line)) + 1), (offset :: results))) in
  Belt.List.reverse bols
let parsingPos offset bols =
  let rec loop i bols =
    match bols with
    | [] -> (i, 0)
    | bol::next::_ when next > offset -> (i, bol)
    | _::rest -> loop (i + 1) rest in
  loop 1 bols
let lexingPos fname offset bols = offset
let convert result fname text =
  let bols = calcBols text in
  match result with
  | ((Node
      (((("Start")[@reason.raw_literal "Start"]),_),children,_,_)))
      ->
      RU.getNodesByType children
        (("Structure")[@reason.raw_literal "Structure"])
        (toOcaml.structure { toOcaml with toLoc = (fun x  -> x) })
  | _ -> failwith ""
end
module OcamlOfReason
= struct
#1 "ocamlOfReason.ml"
[@@@ocaml.ppx.context { cookies = [] }]
open Parsetree
open PackTypes.Result
open Longident
open Location
open Lexing
open Asttypes
module H = Ast_helper
module RU = ResultUtils
let unwrap = ResultUtils.unwrap
let loc = !H.default_loc
let str = H.Str.eval (H.Exp.array [])
type fromOcaml =
  {
  fromStructure: fromOcaml -> structure_item -> result;
  fromExpression: fromOcaml -> expression -> result;}
type loc = PackTypes.Result.loc
type toOcaml =
  {
  expression:
    toOcaml ->
      (string* (string* result) list* loc* PackTypes.Result.comments option)
        -> expression;
  structure:
    toOcaml ->
      (string* (string* result) list* loc* PackTypes.Result.comments option)
        -> structure_item;}
let node a b c = ((Node (a, b, c, None)))
let optOr orr opt =
  match opt with | None  -> orr | ((Some (x))) -> x
let stripRuleName ((name,sub),children,loc,comments) =
  (sub, children, loc, comments)
let mLoc = Location.none
let mLeaf = ((Leaf (("", ""), "", mLoc)))
let isPresent opt = match opt with | Some _ -> true | None  -> false
let stripQuotes str = String.sub str 1 ((String.length str) - 2)
let isUpper x = let n = Char.code (x.[0]) in (90 >= n) && (n >= 65)
let processString str = (str |> stripQuotes) |> Scanf.unescaped
let optFlatMap mapper opt =
  match opt with | ((Some (x))) -> mapper x | None  -> None
let optMap mapper opt =
  match opt with
  | ((Some (x))) -> ((Some ((mapper x))))
  | None  -> None
let getExpression toOcaml children =
  ((RU.getNodeByType children
      (("Expression")[@reason.raw_literal "Expression"]))
     |> unwrap)
    |> (toOcaml.expression toOcaml)
let _parseLongCap children =
  let leafs =
    RU.getChildren children
      (fun (_,child)  ->
         match child with
         | ((Leaf
             (((("capIdent")[@reason.raw_literal "capIdent"]),_),contents,_))
             ) -> ((Some (contents)))
         | _ -> None) in
  let rec loop leafs current =
    match leafs with
    | contents::rest ->
        loop rest ((Ldot (current, contents)))
    | [] -> current in
  match leafs with
  | leftMost::rest -> loop rest ((Lident (leftMost)))
  | [] ->
      failwith (("empty longident")[@reason.raw_literal "empty longident"])
let rec parseLongCap_ (sub,children,_,_) =
  match sub with
  | (("lident")[@reason.raw_literal "lident"]) ->
      ((Lident
          (((RU.getContentsByType children
               (("capIdent")[@reason.raw_literal "capIdent"]))
              |> unwrap))))
  | (("dot")[@reason.raw_literal "dot"]) ->
      ((Ldot
          ((((RU.getNodeByType children
                (("longCap_")[@reason.raw_literal "longCap_"]))
               |> unwrap)
              |> parseLongCap_),
            ((RU.getContentsByType children
                (("capIdent")[@reason.raw_literal "capIdent"]))
               |> unwrap))))
  | _ ->
      failwith
        ((("Invalid longCap_ sub ")[@reason.raw_literal
                                     "Invalid longCap_ sub "])
           ^ sub)
let parseLongCap (_,children,_,_) =
  ((RU.getNodeByType children (("longCap_")[@reason.raw_literal "longCap_"]))
     |> unwrap)
    |> parseLongCap_
let parseLongIdent (_,children,_,_) =
  let first =
    (RU.getNodeByType children (("longCap_")[@reason.raw_literal "longCap_"]))
      |> (optMap parseLongCap_) in
  let last =
    (RU.getContentsByType children
       (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
      |> unwrap in
  match first with
  | ((Some (x))) -> ((Ldot (x, last)))
  | None  -> ((Lident (last)))
let rec fromLongCap_ longident =
  match longident with
  | ((Lident (x))) ->
      node
        ((("longCap_")[@reason.raw_literal "longCap_"]),
          (("lident")[@reason.raw_literal "lident"]))
        [("",
           ((Leaf
               (((("capIdent")[@reason.raw_literal "capIdent"]), ""), x,
                 mLoc))))] mLoc
  | ((Ldot (a,b))) ->
      node
        ((("longCap_")[@reason.raw_literal "longCap_"]),
          (("dot")[@reason.raw_literal "dot"]))
        [("", (fromLongCap_ a));
        ("",
          ((Leaf
              (((("capIdent")[@reason.raw_literal "capIdent"]), ""), b, mLoc))
          ))] mLoc
  | ((Lapply (a,b))) ->
      failwith
        (("long cap can't have an lapply")[@reason.raw_literal
                                            "long cap can't have an lapply"])
let fromLongIdent longident =
  let children =
    match longident with
    | ((Lident (contents))) ->
        [("",
           ((Leaf
               (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
                 contents, mLoc))))]
    | ((Ldot (a,b))) ->
        [("", (fromLongCap_ a));
        ("",
          ((Leaf
              (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""), b,
                mLoc))))]
    | _ ->
        failwith
          (("invalid longident")[@reason.raw_literal "invalid longident"]) in
  node ((("longIdent")[@reason.raw_literal "longIdent"]), "") children mLoc
let fromLongCap longident =
  node ((("longCap")[@reason.raw_literal "longCap"]), "")
    [("", (fromLongCap_ longident))] mLoc
let nodeWrap rulename (sub,children,loc) = node (rulename, sub) children loc
let escapeString text =
  (("\"")[@reason.raw_literal "\\\""]) ^
    ((String.escaped text) ^ (("\"")[@reason.raw_literal "\\\""]))
let parseConstant (sub,children,loc,_comments) =
  let contents =
    (RU.getContentsByLabel children (("val")[@reason.raw_literal "val"])) |>
      unwrap in
  match sub with
  | (("int")[@reason.raw_literal "int"]) ->
      ((Const_int ((int_of_string contents))))
  | (("string")[@reason.raw_literal "string"]) ->
      ((Const_string ((processString contents), None)))
  | (("float")[@reason.raw_literal "float"]) ->
      ((Const_float (contents)))
  | (("char")[@reason.raw_literal "char"]) ->
      ((Const_char (((processString contents).[0]))))
  | _ -> failwith (("nop")[@reason.raw_literal "nop"])
let fromConstant constant =
  match constant with
  | ((Const_int (value))) ->
      ((("int")[@reason.raw_literal "int"]),
        [((("val")[@reason.raw_literal "val"]),
           ((Leaf
               (((("int64")[@reason.raw_literal "int64"]), ""),
                 (string_of_int value), mLoc))))], mLoc)
  | ((Const_string (text,multi))) ->
      ((("string")[@reason.raw_literal "string"]),
        [((("val")[@reason.raw_literal "val"]),
           ((Leaf
               (((("string")[@reason.raw_literal "string"]), ""),
                 (escapeString text), mLoc))))], mLoc)
  | ((Const_float (text))) ->
      ((("float")[@reason.raw_literal "float"]),
        [((("val")[@reason.raw_literal "val"]),
           ((Leaf
               (((("float")[@reason.raw_literal "float"]), ""), text, mLoc))
           ))], mLoc)
  | ((Const_char (chr))) ->
      ((("char")[@reason.raw_literal "char"]),
        [((("val")[@reason.raw_literal "val"]),
           ((Leaf
               (((("char")[@reason.raw_literal "char"]), ""),
                 (Printf.sprintf (("'%c'")[@reason.raw_literal "'%c'"]) chr),
                 mLoc))))], mLoc)
  | _ -> failwith (("unsup const")[@reason.raw_literal "unsup const"])
let ocamlLoc loc = Location.none
let emptyLabeled fn x = ("", (fn x))
let withEmptyLabels x = ("", x)
let labeled label fn x = (label, (fn x))
let rec listToConstruct list maybeRest typeC tupleC =
  match list with
  | [] ->
      (match maybeRest with
       | None  ->
           typeC
             (Location.mkloc
                ((Lident ((("[]")[@reason.raw_literal "[]"])))) loc) None
       | ((Some (x))) -> x)
  | one::rest ->
      typeC
        (Location.mkloc
           ((Lident ((("::")[@reason.raw_literal "::"]))))
           loc)
        ((Some ((tupleC [one; listToConstruct rest maybeRest typeC tupleC])))
        )
let fromPatternRecordItem fromPattern ({ txt = longident;_},pattern) =
  let lident = fromLongIdent longident in
  let children =
    match pattern.ppat_desc with
    | Ppat_any  -> [("", lident)]
    | _ -> [("", lident); ("", (fromPattern pattern))] in
  node ((("PatternRecordItem")[@reason.raw_literal "PatternRecordItem"]), "")
    children mLoc
let rec listFromConstruct ({ ppat_desc;_} as pattern) =
  match ppat_desc with
  | ((Ppat_tuple (first::second::[]))) ->
      ("", (fromPattern first)) :: (listFromConstruct second)
  | ((Ppat_var ({ txt;_}))) ->
      [((("rest")[@reason.raw_literal "rest"]),
         ((Leaf
             (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""), txt,
               mLoc))))]
  | ((Ppat_construct
      ({
         txt = ((Lident
           ((("[]")[@reason.raw_literal "[]"]))));_},_))
      ) -> []
  | ((Ppat_construct
      ({
         txt = ((Lident
           ((("::")[@reason.raw_literal "::"]))));_},((Some
       (pattern))))))
      -> listFromConstruct pattern
  | _ -> [("", (fromPattern pattern))]
and fromPattern { ppat_desc;_} =
  let (sub,children) =
    match ppat_desc with
    | ((Ppat_var ({ txt;_}))) ->
        ((("ident")[@reason.raw_literal "ident"]),
          [("",
             ((Leaf
                 (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
                   txt, mLoc))))])
    | ((Ppat_tuple (items))) ->
        ((("tuple")[@reason.raw_literal "tuple"]),
          (List.map (emptyLabeled fromPattern) items))
    | ((Ppat_construct
        ({
           txt = ((Lident
             ((("[]")[@reason.raw_literal "[]"]))));_},_))
        ) -> ((("list")[@reason.raw_literal "list"]), [])
    | ((Ppat_construct
        ({
           txt = ((Lident
             ((("::")[@reason.raw_literal "::"]))));_},((Some
         (pattern))))))
        ->
        ((("list")[@reason.raw_literal "list"]), (listFromConstruct pattern))
    | Ppat_any  -> ((("ignore")[@reason.raw_literal "ignore"]), [])
    | ((Ppat_constant (constant))) ->
        ((("const")[@reason.raw_literal "const"]),
          [("",
             ((fromConstant constant) |>
                (nodeWrap (("constant")[@reason.raw_literal "constant"]))))])
    | ((Ppat_construct (lid,maybeArg))) ->
        ((("constructor")[@reason.raw_literal "constructor"]),
          (("", (fromLongCap lid.txt)) ::
          ((match maybeArg with
            | None  -> []
            | ((Some
                ({ ppat_desc = ((Ppat_tuple (items)));_}))
                ) ->
                List.map (emptyLabeled fromPattern) items
            | ((Some (pat))) -> [("", (fromPattern pat))]))))
    | ((Ppat_alias (sub,{ txt = name;_}))) ->
        ((("as")[@reason.raw_literal "as"]),
          [("", (fromPattern sub));
          ("",
            ((Leaf
                (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
                  name, mLoc))))])
    | Ppat_interval _ ->
        failwith
          (("nop pat interval")[@reason.raw_literal "nop pat interval"])
    | Ppat_variant _ ->
        failwith (("nop pat variant")[@reason.raw_literal "nop pat variant"])
    | ((Ppat_record (items,closed))) ->
        let children =
          List.map (emptyLabeled (fromPatternRecordItem fromPattern)) items in
        let children =
          match closed = Closed with
          | true  -> children
          | false  ->
              List.concat
                [children;
                [((("open")[@reason.raw_literal "open"]),
                   ((Leaf (("", ""), (("_")[@reason.raw_literal "_"]), mLoc))
                   ))]] in
        ((("record")[@reason.raw_literal "record"]), children)
    | ((Ppat_or (one,two))) ->
        ((("or")[@reason.raw_literal "or"]),
          [("", (fromPattern one)); ("", (fromPattern two))])
    | _ -> failwith (("nop pat")[@reason.raw_literal "nop pat"]) in
  node ((("Pattern")[@reason.raw_literal "Pattern"]), sub) children mLoc
let rec parsePattern toOcaml (sub,children,loc,_comments) =
  let oloc = ocamlLoc loc in
  match sub with
  | (("ident")[@reason.raw_literal "ident"]) ->
      let name =
        (RU.getContentsByType children
           (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
          |> unwrap in
      H.Pat.var (Location.mkloc name oloc)
  | (("tuple")[@reason.raw_literal "tuple"]) ->
      H.Pat.tuple
        (RU.getNodesByType children
           (("Pattern")[@reason.raw_literal "Pattern"])
           (parsePattern toOcaml))
  | (("list")[@reason.raw_literal "list"]) ->
      listToConstruct
        (RU.getNodesByType children
           (("Pattern")[@reason.raw_literal "Pattern"])
           (parsePattern toOcaml))
        ((RU.getContentsByLabel children
            (("rest")[@reason.raw_literal "rest"]))
           |>
           (optFlatMap
              (fun label  ->
                 ((Some ((H.Pat.var (Location.mkloc label oloc))))))))
        H.Pat.construct H.Pat.tuple
  | (("const")[@reason.raw_literal "const"]) ->
      H.Pat.constant
        (((RU.getNodeByType children
             (("constant")[@reason.raw_literal "constant"]))
            |> unwrap)
           |> parseConstant)
  | (("ignore")[@reason.raw_literal "ignore"]) -> H.Pat.any ()
  | (("constructor")[@reason.raw_literal "constructor"]) ->
      H.Pat.construct
        ((((RU.getNodeByType children
              (("longCap")[@reason.raw_literal "longCap"]))
             |> unwrap)
            |> parseLongCap)
           |> Location.mknoloc)
        (match RU.getNodesByType children
                 (("Pattern")[@reason.raw_literal "Pattern"])
                 (parsePattern toOcaml)
         with
         | [] -> None
         | arg::[] -> ((Some (arg)))
         | args -> ((Some ((H.Pat.tuple args)))))
  | _ ->
      failwith
        ((("not impl pattern stuff")[@reason.raw_literal
                                      "not impl pattern stuff"])
           ^ sub)
let parseModuleDesc toOcaml (sub,children,loc,_comments) =
  match sub with
  | (("structure")[@reason.raw_literal "structure"]) ->
      ((Pmod_structure
          ((RU.getNodesByType children
              (("Structure")[@reason.raw_literal "Structure"])
              (toOcaml.structure toOcaml)))))
  | (("ident")[@reason.raw_literal "ident"]) ->
      let ident =
        ((RU.getNodeByLabel children (("ident")[@reason.raw_literal "ident"]))
           |> unwrap)
          |> parseLongCap in
      ((Pmod_ident ((Location.mkloc ident (ocamlLoc loc)))))
  | _ -> failwith (("not impl")[@reason.raw_literal "not impl"])
let fromModuleDesc fromOcaml desc =
  match desc with
  | ((Pmod_structure (items))) ->
      node
        ((("ModuleDesc")[@reason.raw_literal "ModuleDesc"]),
          (("structure")[@reason.raw_literal "structure"]))
        (List.map (emptyLabeled (fromOcaml.fromStructure fromOcaml)) items)
        mLoc
  | ((Pmod_ident ({ txt;_}))) ->
      node
        ((("ModuleDesc")[@reason.raw_literal "ModuleDesc"]),
          (("ident")[@reason.raw_literal "ident"]))
        [((("ident")[@reason.raw_literal "ident"]), (fromLongCap txt))] mLoc
  | ((Pmod_apply (expr,arg))) ->
      failwith
        (("nop module desc appply")[@reason.raw_literal
                                     "nop module desc appply"])
  | _ ->
      failwith
        (("module desc not imprt")[@reason.raw_literal
                                    "module desc not imprt"])
let rec fromCoreType { ptyp_desc = desc;_} =
  let (sub,children) =
    match desc with
    | Ptyp_any  -> ((("any")[@reason.raw_literal "any"]), [])
    | ((Ptyp_var (text))) ->
        ((("var")[@reason.raw_literal "var"]),
          [("",
             ((Leaf
                 (((("typeVar")[@reason.raw_literal "typeVar"]), ""), text,
                   mLoc))))])
    | ((Ptyp_arrow (label,in_type,out_type))) ->
        let children =
          [((("in")[@reason.raw_literal "in"]), (fromCoreType in_type));
          ((("out")[@reason.raw_literal "out"]), (fromCoreType out_type))] in
        let children =
          if label = ""
          then children
          else
            (let (text,opt) =
               if (label.[0]) = '?'
               then ((String.sub label 1 ((String.length label) - 1)), true)
               else (label, false) in
             let children =
               if opt
               then
                 [((("optional")[@reason.raw_literal "optional"]),
                    ((Leaf (("", ""), (("?")[@reason.raw_literal "?"]), mLoc))
                    ))]
               else [] in
             ("",
               ((Leaf
                   (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
                     text, mLoc))))
               :: children) in
        ((("fn")[@reason.raw_literal "fn"]), children)
    | ((Ptyp_tuple (types))) ->
        ((("tuple")[@reason.raw_literal "tuple"]),
          (List.map (emptyLabeled fromCoreType) types))
    | ((Ptyp_constr (ident,args))) ->
        ((("constructor")[@reason.raw_literal "constructor"]),
          (("", (fromLongIdent ident.txt)) ::
          (List.map (emptyLabeled fromCoreType) args)))
    | Ptyp_object _ ->
        failwith (("no object")[@reason.raw_literal "no object"])
    | Ptyp_class _ -> failwith (("no class")[@reason.raw_literal "no class"])
    | ((Ptyp_alias (typ,name))) ->
        ((("alias")[@reason.raw_literal "alias"]),
          [("", (fromCoreType typ));
          ("",
            ((Leaf
                (((("typeVar")[@reason.raw_literal "typeVar"]), ""), name,
                  mLoc))))])
    | Ptyp_variant _ ->
        failwith (("no variant")[@reason.raw_literal "no variant"])
    | Ptyp_poly _ -> failwith (("no poly")[@reason.raw_literal "no poly"])
    | Ptyp_package _ ->
        failwith (("no package")[@reason.raw_literal "no package"])
    | Ptyp_extension _ ->
        failwith (("no exptesion")[@reason.raw_literal "no exptesion"]) in
  node ((("Type")[@reason.raw_literal "Type"]), sub) children mLoc
let rec toCoreType (sub,children,loc,_comments) =
  match sub with
  | (("any")[@reason.raw_literal "any"]) -> H.Typ.any ()
  | (("var")[@reason.raw_literal "var"]) ->
      H.Typ.var
        ((RU.getContentsByType children
            (("typeVar")[@reason.raw_literal "typeVar"]))
           |> unwrap)
  | (("fn")[@reason.raw_literal "fn"]) ->
      let isOpt =
        RU.getPresenceByLabel children
          (("optional")[@reason.raw_literal "optional"]) in
      let label =
        (RU.getContentsByType children
           (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
          |> (optOr "") in
      let label =
        match isOpt with
        | true  -> (("?")[@reason.raw_literal "?"]) ^ label
        | false  -> label in
      let in_type =
        (((RU.getNodeByLabel children (("in")[@reason.raw_literal "in"])) |>
            unwrap)
           |> stripRuleName)
          |> toCoreType in
      let out_type =
        (((RU.getNodeByLabel children (("out")[@reason.raw_literal "out"]))
            |> unwrap)
           |> stripRuleName)
          |> toCoreType in
      H.Typ.arrow label in_type out_type
  | (("tuple")[@reason.raw_literal "tuple"]) ->
      H.Typ.tuple
        (RU.getNodesByType children (("Type")[@reason.raw_literal "Type"])
           toCoreType)
  | (("constructor")[@reason.raw_literal "constructor"]) ->
      H.Typ.constr
        ((((RU.getNodeByType children
              (("longIdent")[@reason.raw_literal "longIdent"]))
             |> unwrap)
            |> parseLongIdent)
           |> Location.mknoloc)
        (RU.getNodesByType children (("Type")[@reason.raw_literal "Type"])
           toCoreType)
  | (("alias")[@reason.raw_literal "alias"]) ->
      H.Typ.alias
        (((RU.getNodeByType children (("Type")[@reason.raw_literal "Type"]))
            |> unwrap)
           |> toCoreType)
        ((RU.getContentsByType children
            (("typeVar")[@reason.raw_literal "typeVar"]))
           |> unwrap)
  | _ ->
      failwith
        ((("to coretype not impl ")[@reason.raw_literal
                                     "to coretype not impl "])
           ^ sub)
let fromTypeVariantItem { pcd_name = name; pcd_args = args } =
  node ((("TypeVariantItem")[@reason.raw_literal "TypeVariantItem"]), "")
    (("",
       ((Leaf
           (((("capIdent")[@reason.raw_literal "capIdent"]), ""), (name.txt),
             mLoc)))) ::
    (List.map (emptyLabeled fromCoreType) args)) mLoc
let fromTypeRecordItem { pld_name = name; pld_type = typ } =
  node ((("TypeRecordItem")[@reason.raw_literal "TypeRecordItem"]), "")
    [("",
       ((Leaf
           (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
             (name.txt), mLoc))));
    ("", (fromCoreType typ))] mLoc
let fromTypeDeclaration fromOcaml
  { ptype_kind = kind; ptype_name = name; ptype_manifest = manifest;_} =
  let nameLeaf =
    ((Leaf
        (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
          (name.txt), mLoc))) in
  let (sub,children) =
    match kind with
    | Ptype_abstract  ->
        (match manifest with
         | None  ->
             ((("ident")[@reason.raw_literal "ident"]), [("", nameLeaf)])
         | ((Some (core_type))) ->
             ((("abs_manifest")[@reason.raw_literal "abs_manifest"]),
               [("", nameLeaf); ("", (fromCoreType core_type))]))
    | ((Ptype_variant (constructors))) ->
        let tvar =
          node
            ((("TypeDecl")[@reason.raw_literal "TypeDecl"]),
              (("variant")[@reason.raw_literal "variant"]))
            [("",
               (node
                  ((("TypeVariant")[@reason.raw_literal "TypeVariant"]), "")
                  (List.map (emptyLabeled fromTypeVariantItem) constructors)
                  mLoc))] mLoc in
        (match manifest with
         | None  ->
             ((("decl")[@reason.raw_literal "decl"]),
               [("", nameLeaf); ("", tvar)])
         | ((Some (core_type))) ->
             ((("manifested")[@reason.raw_literal "manifested"]),
               [("", nameLeaf); ("", (fromCoreType core_type)); ("", tvar)]))
    | ((Ptype_record (label_declarations))) ->
        let trec =
          node
            ((("TypeDecl")[@reason.raw_literal "TypeDecl"]),
              (("record")[@reason.raw_literal "record"]))
            (List.map (emptyLabeled fromTypeRecordItem) label_declarations)
            mLoc in
        (match manifest with
         | None  ->
             ((("decl")[@reason.raw_literal "decl"]),
               [("", nameLeaf); ("", trec)])
         | ((Some (core_type))) ->
             ((("manifested")[@reason.raw_literal "manifested"]),
               [("", nameLeaf); ("", (fromCoreType core_type)); ("", trec)]))
    | Ptype_open  ->
        failwith (("nop open types")[@reason.raw_literal "nop open types"]) in
  node ((("TypeDeclaration")[@reason.raw_literal "TypeDeclaration"]), sub)
    children mLoc
let justChildren (_,children,_,_) = children
let parseTypeVariantItem (sub,children,loc,_comments) =
  let name =
    (RU.getContentsByType children
       (("capIdent")[@reason.raw_literal "capIdent"]))
      |> unwrap in
  let args =
    RU.getNodesByType children (("Type")[@reason.raw_literal "Type"])
      toCoreType in
  H.Type.constructor ~args (Location.mknoloc name)
let parseTypeRecordItem (sub,children,loc,_comments) =
  let name =
    (RU.getContentsByType children
       (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
      |> unwrap in
  let typ =
    ((RU.getNodeByType children (("Type")[@reason.raw_literal "Type"])) |>
       unwrap)
      |> toCoreType in
  H.Type.field (Location.mknoloc name) typ
let parseTypeDecl (sub,children,loc,_comments) =
  match sub with
  | (("variant")[@reason.raw_literal "variant"]) ->
      let children =
        ((RU.getNodeByType children
            (("TypeVariant")[@reason.raw_literal "TypeVariant"]))
           |> unwrap)
          |> justChildren in
      ((Ptype_variant
          ((RU.getNodesByType children
              (("TypeVariantItem")[@reason.raw_literal "TypeVariantItem"])
              parseTypeVariantItem))))
  | (("record")[@reason.raw_literal "record"]) ->
      ((Ptype_record
          ((RU.getNodesByType children
              (("TypeRecordItem")[@reason.raw_literal "TypeRecordItem"])
              parseTypeRecordItem))))
  | _ ->
      failwith
        (("invalid type decl")[@reason.raw_literal "invalid type decl"])
let parseTypeDeclaration toOcaml (sub,children,loc,_comments) =
  let name =
    Location.mknoloc
      ((RU.getContentsByType children
          (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
         |> unwrap) in
  match sub with
  | (("ident")[@reason.raw_literal "ident"]) ->
      H.Type.mk ~kind:Ptype_abstract name
  | (("abs_manifest")[@reason.raw_literal "abs_manifest"]) ->
      H.Type.mk ~kind:Ptype_abstract
        ~manifest:(((RU.getNodeByType children
                       (("Type")[@reason.raw_literal "Type"]))
                      |> unwrap)
                     |> toCoreType) name
  | (("decl")[@reason.raw_literal "decl"]) ->
      H.Type.mk
        ~kind:(((RU.getNodeByType children
                   (("TypeDecl")[@reason.raw_literal "TypeDecl"]))
                  |> unwrap)
                 |> parseTypeDecl) name
  | (("manifested")[@reason.raw_literal "manifested"]) ->
      H.Type.mk
        ~manifest:(((RU.getNodeByType children
                       (("Type")[@reason.raw_literal "Type"]))
                      |> unwrap)
                     |> toCoreType)
        ~kind:(((RU.getNodeByType children
                   (("TypeDecl")[@reason.raw_literal "TypeDecl"]))
                  |> unwrap)
                 |> parseTypeDecl) name
  | _ ->
      failwith
        (("nope type declaration")[@reason.raw_literal
                                    "nope type declaration"])
let parseArgValue toOcaml (sub,children,loc,_comments) =
  match sub with
  | (("none")[@reason.raw_literal "none"]) -> None
  | (("expr")[@reason.raw_literal "expr"]) ->
      ((Some
          ((((RU.getNodeByType children
                (("Expression")[@reason.raw_literal "Expression"]))
               |> unwrap)
              |> (toOcaml.expression toOcaml)))))
  | _ ->
      failwith
        (("unexpected argv value")[@reason.raw_literal
                                    "unexpected argv value"])
let fromArgValue fromOcaml maybeDefault =
  let (sub,children) =
    match maybeDefault with
    | None  -> ((("none")[@reason.raw_literal "none"]), [])
    | ((Some (x))) ->
        ((("expr")[@reason.raw_literal "expr"]),
          [("", (fromOcaml.fromExpression fromOcaml x))]) in
  node ((("ArgValue")[@reason.raw_literal "ArgValue"]), sub) children mLoc
let parseArg toOcaml (sub,children,loc,_comments) =
  let oloc = ocamlLoc loc in
  match sub with
  | (("punned")[@reason.raw_literal "punned"]) ->
      let name =
        (RU.getContentsByType children
           (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
          |> unwrap in
      let maybeArg =
        RU.getNodeByType children
          (("ArgValue")[@reason.raw_literal "ArgValue"]) in
      let maybeExpr = maybeArg |> (optFlatMap (parseArgValue toOcaml)) in
      let name =
        match maybeArg = None with
        | true  -> name
        | false  -> (("?")[@reason.raw_literal "?"]) ^ name in
      (name, (H.Pat.var (Location.mkloc name oloc)), maybeExpr)
  | (("anon")[@reason.raw_literal "anon"]) ->
      ("",
        (((RU.getNodeByType children
             (("Pattern")[@reason.raw_literal "Pattern"]))
            |> unwrap)
           |> (parsePattern toOcaml)), None)
  | (("named")[@reason.raw_literal "named"]) ->
      let name =
        (RU.getContentsByType children
           (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
          |> unwrap in
      let pat =
        ((RU.getNodeByType children
            (("Pattern")[@reason.raw_literal "Pattern"]))
           |> unwrap)
          |> (parsePattern toOcaml) in
      let maybeArg =
        RU.getNodeByType children
          (("ArgValue")[@reason.raw_literal "ArgValue"]) in
      let maybeExpr = maybeArg |> (optFlatMap (parseArgValue toOcaml)) in
      let name =
        match maybeArg = None with
        | true  -> name
        | false  -> (("?")[@reason.raw_literal "?"]) ^ name in
      (name, pat, maybeExpr)
  | _ -> failwith (("nop arg")[@reason.raw_literal "nop arg"])
let fromArg fromOcaml (label,maybeDefault,pattern) =
  let (sub,children) =
    if label = ""
    then
      ((("anon")[@reason.raw_literal "anon"]), [("", (fromPattern pattern))])
    else
      (let ll = String.length label in
       let (label,opt) =
         if (ll > 0) && ((label.[0]) = '?')
         then ((String.sub label 1 (ll - 1)), true)
         else (label, false) in
       let argValue =
         match opt with
         | true  -> [("", (fromArgValue fromOcaml maybeDefault))]
         | false  -> [] in
       let ident =
         ((Leaf
             (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
               label, mLoc))) in
       match pattern.ppat_desc with
       | ((Ppat_var ({ txt;_}))) when txt = label ->
           ((("punned")[@reason.raw_literal "punned"]), (("", ident) ::
             argValue))
       | _ ->
           ((("named")[@reason.raw_literal "named"]), (("", ident) ::
             ("", (fromPattern pattern)) :: argValue))) in
  node ((("Arg")[@reason.raw_literal "Arg"]), sub) children mLoc
let makeFunction toOcaml args expr =
  List.fold_left
    (fun expr  ->
       fun (label,pat,maybeExpr)  -> H.Exp.fun_ label maybeExpr pat expr)
    expr args
let parseBinding toOcaml (sub,children,loc,_comments) =
  let pvb_loc = ocamlLoc loc in
  match sub with
  | (("func")[@reason.raw_literal "func"]) ->
      let name =
        (RU.getContentsByLabel children
           (("name")[@reason.raw_literal "name"]))
          |> unwrap in
      let args =
        RU.getNodesByType children (("Arg")[@reason.raw_literal "Arg"])
          (parseArg toOcaml) in
      let expr =
        ((RU.getNodeByType children
            (("Expression")[@reason.raw_literal "Expression"]))
           |> unwrap)
          |> (toOcaml.expression toOcaml) in
      let pvb_expr = makeFunction toOcaml args expr in
      {
        pvb_pat = (H.Pat.var (Location.mkloc name pvb_loc));
        pvb_expr;
        pvb_attributes = [];
        pvb_loc
      }
  | (("value")[@reason.raw_literal "value"]) ->
      let pvb_pat =
        ((RU.getNodeByType children
            (("Pattern")[@reason.raw_literal "Pattern"]))
           |> unwrap)
          |> (parsePattern toOcaml) in
      let pvb_expr =
        ((RU.getNodeByType children
            (("Expression")[@reason.raw_literal "Expression"]))
           |> unwrap)
          |> (toOcaml.expression toOcaml) in
      { pvb_pat; pvb_expr; pvb_attributes = []; pvb_loc }
  | _ ->
      failwith (("unknown binding")[@reason.raw_literal "unknown binding"])
let fromValueBinding fromOcaml { pvb_pat; pvb_expr;_} =
  node
    ((("ValueBinding")[@reason.raw_literal "ValueBinding"]),
      (("value")[@reason.raw_literal "value"]))
    [("", (fromPattern pvb_pat));
    ("", (fromOcaml.fromExpression fromOcaml pvb_expr))] mLoc
let rec parseType (sub,children,loc,_) =
  let oloc = ocamlLoc loc in
  match sub with
  | (("constructor")[@reason.raw_literal "constructor"]) ->
      let ident =
        ((RU.getNodeByType children
            (("longIdent")[@reason.raw_literal "longIdent"]))
           |> unwrap)
          |> parseLongIdent in
      let types =
        RU.getNodesByType children (("Type")[@reason.raw_literal "Type"])
          parseType in
      H.Typ.constr (Location.mkloc ident oloc) types
  | _ ->
      failwith
        (("not support atm type")[@reason.raw_literal "not support atm type"])
let parseStructure toOcaml (sub,children,loc,_) =
  let oloc = ocamlLoc loc in
  match sub with
  | (("let_module")[@reason.raw_literal "let_module"]) ->
      H.Str.module_
        {
          pmb_name =
            (Location.mkloc
               ((RU.getContentsByType children
                   (("capIdent")[@reason.raw_literal "capIdent"]))
                  |> unwrap) oloc);
          pmb_attributes = [];
          pmb_loc = oloc;
          pmb_expr =
            {
              pmod_desc =
                (((RU.getNodeByType children
                     (("ModuleDesc")[@reason.raw_literal "ModuleDesc"]))
                    |> unwrap)
                   |> (parseModuleDesc toOcaml));
              pmod_loc = oloc;
              pmod_attributes = []
            }
        }
  | (("value")[@reason.raw_literal "value"]) ->
      let isRec =
        RU.getPresenceByLabel children (("rec")[@reason.raw_literal "rec"]) in
      let bindings =
        RU.getNodesByType children
          (("ValueBinding")[@reason.raw_literal "ValueBinding"])
          (parseBinding toOcaml) in
      H.Str.value
        (match isRec with | true  -> Recursive | false  -> Nonrecursive)
        bindings
  | (("eval")[@reason.raw_literal "eval"]) ->
      H.Str.eval
        (((RU.getNodeByType children
             (("Expression")[@reason.raw_literal "Expression"]))
            |> unwrap)
           |> (toOcaml.expression toOcaml))
  | (("type")[@reason.raw_literal "type"]) ->
      H.Str.type_
        (RU.getNodesByType children
           (("TypeDeclaration")[@reason.raw_literal "TypeDeclaration"])
           (parseTypeDeclaration toOcaml))
  | (("open")[@reason.raw_literal "open"]) ->
      H.Str.open_
        {
          popen_lid =
            (Location.mkloc
               (((RU.getNodeByType children
                    (("longCap")[@reason.raw_literal "longCap"]))
                   |> unwrap)
                  |> parseLongCap) oloc);
          popen_override = Fresh;
          popen_loc = oloc;
          popen_attributes = []
        }
  | _ ->
      failwith
        ((("Unknown structure type - ")[@reason.raw_literal
                                         "Unknown structure type - "])
           ^ sub)
let fromStructure fromOcaml structure =
  match structure.pstr_desc with
  | ((Pstr_value (recFlag,valueBindings))) ->
      let children =
        List.map (emptyLabeled (fromValueBinding fromOcaml)) valueBindings in
      let children =
        match recFlag = Recursive with
        | true  -> ((("rec")[@reason.raw_literal "rec"]), mLeaf) :: children
        | false  -> children in
      node
        ((("Structure")[@reason.raw_literal "Structure"]),
          (("value")[@reason.raw_literal "value"])) children mLoc
  | ((Pstr_eval (expr,attrs))) ->
      node
        ((("Structure")[@reason.raw_literal "Structure"]),
          (("eval")[@reason.raw_literal "eval"]))
        [("", (fromOcaml.fromExpression fromOcaml expr))] mLoc
  | ((Pstr_module
      ({ pmb_name = { txt;_}; pmb_expr = { pmod_desc;_};_})))
      ->
      node
        ((("Structure")[@reason.raw_literal "Structure"]),
          (("let_module")[@reason.raw_literal "let_module"]))
        [("",
           ((Leaf
               (((("capIdent")[@reason.raw_literal "capIdent"]), ""), txt,
                 mLoc))));
        ("", (fromModuleDesc fromOcaml pmod_desc))] mLoc
  | ((Pstr_open ({ popen_lid;_}))) ->
      node
        ((("Structure")[@reason.raw_literal "Structure"]),
          (("open")[@reason.raw_literal "open"]))
        [("", (fromLongCap popen_lid.txt))] mLoc
  | ((Pstr_type (declarations))) ->
      node
        ((("Structure")[@reason.raw_literal "Structure"]),
          (("type")[@reason.raw_literal "type"]))
        (List.map (emptyLabeled (fromTypeDeclaration fromOcaml)) declarations)
        mLoc
  | ((Pstr_exception
      ({ pext_name = { txt = name }; pext_kind;_}))) ->
      (match pext_kind with
       | ((Pext_decl (args,manifest))) ->
           node
             ((("Structure")[@reason.raw_literal "Structure"]),
               (("exception")[@reason.raw_literal "exception"]))
             (("",
                ((Leaf
                    (((("capIdent")[@reason.raw_literal "capIdent"]), ""),
                      name, mLoc)))) ::
             (List.map (emptyLabeled fromCoreType) args)) mLoc
       | Pext_rebind _ ->
           failwith (("no ext rebind")[@reason.raw_literal "no ext rebind"]))
  | _ ->
      (Printast.structure 0 Format.std_formatter [structure];
       failwith
         (("no parse structure")[@reason.raw_literal "no parse structure"]))
let parseFnArg toOcaml (sub,children,loc,_comments) =
  let oloc = ocamlLoc loc in
  match sub with
  | (("punned")[@reason.raw_literal "punned"]) ->
      let name =
        (RU.getContentsByType children
           (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
          |> unwrap in
      (name,
        (H.Exp.ident
           (Location.mkloc ((Lident (name))) oloc)))
  | (("named")[@reason.raw_literal "named"]) ->
      let name =
        (RU.getContentsByType children
           (("lowerIdent")[@reason.raw_literal "lowerIdent"]))
          |> unwrap in
      let value =
        ((RU.getNodeByType children
            (("Expression")[@reason.raw_literal "Expression"]))
           |> unwrap)
          |> (toOcaml.expression toOcaml) in
      (name, value)
  | (("anon")[@reason.raw_literal "anon"]) ->
      let value =
        ((RU.getNodeByType children
            (("Expression")[@reason.raw_literal "Expression"]))
           |> unwrap)
          |> (toOcaml.expression toOcaml) in
      ("", value)
  | _ ->
      failwith
        ((("unknown fnarg type ")[@reason.raw_literal "unknown fnarg type "])
           ^ sub)
let fromFnArg fromOcaml (label,arg) =
  match arg.pexp_desc with
  | ((Pexp_ident
      ({ txt = ((Lident (name)));_})))
      when name = label ->
      node
        ((("FnArg")[@reason.raw_literal "FnArg"]),
          (("punned")[@reason.raw_literal "punned"]))
        [("",
           ((Leaf
               (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
                 name, mLoc))))] mLoc
  | _ ->
      let exp = fromOcaml.fromExpression fromOcaml arg in
      if label = ""
      then
        node
          ((("FnArg")[@reason.raw_literal "FnArg"]),
            (("anon")[@reason.raw_literal "anon"])) [("", exp)] mLoc
      else
        node
          ((("FnArg")[@reason.raw_literal "FnArg"]),
            (("named")[@reason.raw_literal "named"]))
          [("",
             ((Leaf
                 (((("lowerIdent")[@reason.raw_literal "lowerIdent"]), ""),
                   label, mLoc))));
          ("", exp)] mLoc
let rec unrollFunExpr label maybeDefault pattern expr =
  let arg = (label, maybeDefault, pattern) in
  match expr.pexp_desc with
  | ((Pexp_fun (l,m,p,e))) ->
      let (rest,exp) = unrollFunExpr l m p e in ((arg :: rest), exp)
  | _ -> ([arg], expr)
let fromFunExpr fromOcaml label maybeDefault pattern expr =
  let (args,exp) = unrollFunExpr label maybeDefault pattern expr in
  let sub =
    match args with
    | one::[] -> (("single")[@reason.raw_literal "single"])
    | _ -> (("multi")[@reason.raw_literal "multi"]) in
  node ((("FunExpr")[@reason.raw_literal "FunExpr"]), sub)
    (List.concat
       [List.map (emptyLabeled (fromArg fromOcaml)) args;
       [("", (fromOcaml.fromExpression fromOcaml exp))]]) mLoc
let parseFunExpr toOcaml (sub,children,loc,_comments) =
  let args =
    RU.getNodesByType children (("Arg")[@reason.raw_literal "Arg"])
      (parseArg toOcaml) in
  let expr =
    ((RU.getNodeByType children
        (("Expression")[@reason.raw_literal "Expression"]))
       |> unwrap)
      |> (toOcaml.expression toOcaml) in
  makeFunction toOcaml args expr
let unwrapm message opt =
  match opt with
  | ((Some (x))) -> x
  | None  ->
      raise
        ((RU.ConversionFailure
            (((("Unwrapping none ")[@reason.raw_literal "Unwrapping none "])
                ^ message))))
let fromLet fromOcaml isRec values =
  let bindings = List.map (emptyLabeled (fromValueBinding fromOcaml)) values in
  node
    ((("Statement")[@reason.raw_literal "Statement"]),
      (("value")[@reason.raw_literal "value"]))
    (match isRec = Recursive with
     | true  ->
         ((("rec")[@reason.raw_literal "rec"]),
           ((Leaf (("", ""), (("rec")[@reason.raw_literal "rec"]), mLoc))
           ))
         :: bindings
     | false  -> bindings) mLoc
let rec unwrapSequence fromOcaml exp =
  match exp.pexp_desc with
  | ((Pexp_sequence (first,second))) ->
      List.concat
        [unwrapSequence fromOcaml first; unwrapSequence fromOcaml second]
  | ((Pexp_let (isRec,values,exp))) ->
      (fromLet fromOcaml isRec values) :: (unwrapSequence fromOcaml exp)
  | ((Pexp_letmodule ({ txt;_},modexp,exp))) ->
      failwith
        (("letmodule not yet")[@reason.raw_literal "letmodule not yet"])
  | _ ->
      [node
         ((("Statement")[@reason.raw_literal "Statement"]),
           (("expr")[@reason.raw_literal "expr"]))
         [("", (fromOcaml.fromExpression fromOcaml exp))] mLoc]
let stringToIdentLoc loc txt =
  Location.mkloc ((Lident (txt))) loc
let parseBlock toOcaml (sub,children,loc,_comments) =
  let oloc = ocamlLoc loc in
  let rec loop children =
    match children with
    | [] ->
        H.Exp.ident
          (Location.mkloc
             ((Lident ((("()")[@reason.raw_literal "()"]))))
             oloc)
    | (_,Leaf _)::rest -> loop rest
    | (_,((Node
       (((("Statement")[@reason.raw_literal "Statement"]),(("expr")[@reason.raw_literal
                                                                    "expr"])),children,_,_))
       ))::[] -> getExpression toOcaml children
    | (_,((Node
       (((("Statement")[@reason.raw_literal "Statement"]),(("expr")[@reason.raw_literal
                                                                    "expr"])),children,_,_))
       ))::rest ->
        H.Exp.sequence (getExpression toOcaml children) (loop rest)
    | (_,((Node
       (((("Statement")[@reason.raw_literal "Statement"]),(("value")[@reason.raw_literal
                                                                    "value"])),children,_,_))
       ))::rest ->
        let isRec =
          RU.getPresenceByLabel children (("rec")[@reason.raw_literal "rec"]) in
        let bindings =
          RU.getNodesByType children
            (("ValueBinding")[@reason.raw_literal "ValueBinding"])
            (parseBinding toOcaml) in
        H.Exp.let_
          (match isRec with | true  -> Recursive | false  -> Nonrecursive)
          bindings (loop rest)
    | (_,((Node
       (((("Statement")[@reason.raw_literal "Statement"]),(("module")
         [@reason.raw_literal "module"])),children,_,_))))::rest
        ->
        let name =
          Location.mkloc
            ((RU.getContentsByType children
                (("capIdent")[@reason.raw_literal "capIdent"]))
               |> unwrap) oloc in
        H.Exp.letmodule name
          {
            pmod_desc =
              (((RU.getNodeByType children
                   (("ModuleDesc")[@reason.raw_literal "ModuleDesc"]))
                  |> unwrap)
                 |> (parseModuleDesc toOcaml));
            pmod_loc = oloc;
            pmod_attributes = []
          } (loop rest)
    | _ ->
        failwith
          (("Unknown statement")[@reason.raw_literal "Unknown statement"]) in
  loop children
let fromBlock fromOcaml expr =
  match expr.pexp_desc with
  | ((Pexp_let (isRec,values,exp))) ->
      let children = (fromLet fromOcaml isRec values) ::
        (unwrapSequence fromOcaml exp) in
      node ((("Block")[@reason.raw_literal "Block"]), "")
        (List.map withEmptyLabels children) mLoc
  | ((Pexp_sequence (first,second))) ->
      let children =
        List.concat
          [unwrapSequence fromOcaml first; unwrapSequence fromOcaml second] in
      node ((("Block")[@reason.raw_literal "Block"]), "")
        (List.map withEmptyLabels children) mLoc
  | _ ->
      let node_ = fromOcaml.fromExpression fromOcaml expr in
      node ((("Block")[@reason.raw_literal "Block"]), "")
        [("",
           (node
              ((("Statement")[@reason.raw_literal "Statement"]),
                (("expr")[@reason.raw_literal "expr"])) [("", node_)] mLoc))]
        mLoc
let parseSwitchCase toOcaml (sub,children,loc,_comments) =
  let pattern =
    ((RU.getNodeByType children (("Pattern")[@reason.raw_literal "Pattern"]))
       |> unwrap)
      |> (parsePattern toOcaml) in
  let guard =
    ((RU.getNodeByLabel children (("guard")[@reason.raw_literal "guard"])) |>
       (optMap stripRuleName))
      |> (optMap (toOcaml.expression toOcaml)) in
  let body =
    (((RU.getNodeByLabel children (("body")[@reason.raw_literal "body"])) |>
        unwrap)
       |> stripRuleName)
      |> (toOcaml.expression toOcaml) in
  { pc_lhs = pattern; pc_guard = guard; pc_rhs = body }
let parseSwitchExp toOcaml (sub,children,loc,_comments) =
  let base =
    ((RU.getNodeByType children
        (("Expression")[@reason.raw_literal "Expression"]))
       |> unwrap)
      |> (toOcaml.expression toOcaml) in
  let cases =
    RU.getNodesByType children
      (("SwitchCase")[@reason.raw_literal "SwitchCase"])
      (parseSwitchCase toOcaml) in
  H.Exp.match_ base cases
let fromSwitchCase fromOcaml { pc_lhs; pc_guard; pc_rhs } =
  let pattern = fromPattern pc_lhs in
  let guard = pc_guard |> (optMap (fromOcaml.fromExpression fromOcaml)) in
  let body = pc_rhs |> (fromOcaml.fromExpression fromOcaml) in
  let children =
    match guard with
    | None  ->
        [("", pattern); ((("body")[@reason.raw_literal "body"]), body)]
    | ((Some (guard))) ->
        [("", pattern);
        ((("guard")[@reason.raw_literal "guard"]), guard);
        ((("body")[@reason.raw_literal "body"]), body)] in
  node ((("SwitchCase")[@reason.raw_literal "SwitchCase"]), "") children mLoc
let fromSwitchExp fromOcaml base cases =
  let base = fromOcaml.fromExpression fromOcaml base in
  let cases = List.map (fromSwitchCase fromOcaml) cases in
  ((("switch")[@reason.raw_literal "switch"]),
    [("",
       (node ((("SwitchExp")[@reason.raw_literal "SwitchExp"]), "")
          (("", base) :: (List.map withEmptyLabels cases)) mLoc))])
let parseIfExp toOcaml (sub,children,loc,_comments) =
  let conditions =
    RU.getNodesByType children
      (("Expression")[@reason.raw_literal "Expression"])
      (toOcaml.expression toOcaml) in
  let consequents =
    RU.getNodesByType children (("Block")[@reason.raw_literal "Block"])
      (parseBlock toOcaml) in
  let rec loop exprs blocks =
    match (exprs, blocks) with
    | ([],elseblock::[]) -> ((Some (elseblock)))
    | ([],[]) -> None
    | (expr::exprs,block::blocks) ->
        ((Some ((H.Exp.ifthenelse expr block (loop exprs blocks)))))
    | _ ->
        failwith
          (("Invalid ifthenelse")[@reason.raw_literal "Invalid ifthenelse"]) in
  (loop conditions consequents) |> unwrap
let fromIfExp fromOcaml (cond,cons,maybeAlt) =
  let rec loop ({ pexp_desc;_} as expression) =
    match pexp_desc with
    | ((Pexp_ifthenelse (cond,cons,maybeAlt))) ->
        (match maybeAlt with
         | None  -> ([cond], [cons])
         | ((Some (alt))) ->
             let (exps,blocks) = loop alt in
             ((cond :: exps), (cons :: blocks)))
    | _ -> ([], [expression]) in
  let (conds,blocks) =
    match maybeAlt with
    | ((Some (alternate))) -> loop alternate
    | None  -> ([], []) in
  let conds =
    (cond :: conds) |> (List.map (fromOcaml.fromExpression fromOcaml)) in
  let blocks = (cons :: blocks) |> (List.map (fromBlock fromOcaml)) in
  let children = List.map withEmptyLabels (List.concat [conds; blocks]) in
  ((("if")[@reason.raw_literal "if"]),
    [("",
       (node ((("IfExpr")[@reason.raw_literal "IfExpr"]), "") children mLoc))])
let parseTry toOcaml (_,children,loc,_) =
  let block =
    ((RU.getNodeByType children (("Block")[@reason.raw_literal "Block"])) |>
       unwrap)
      |> (parseBlock toOcaml) in
  let cases =
    RU.getNodesByType children
      (("SwitchCase")[@reason.raw_literal "SwitchCase"])
      (parseSwitchCase toOcaml) in
  H.Exp.try_ block cases
let fromTry fromOcaml block cases =
  let block = block |> (fromBlock fromOcaml) in
  let cases = List.map (emptyLabeled (fromSwitchCase fromOcaml)) cases in
  ((("try")[@reason.raw_literal "try"]),
    [("",
       (node ((("TryExp")[@reason.raw_literal "TryExp"]), "") (("", block) ::
          cases) mLoc))])
let parseConstructor toOcaml children =
  let lid =
    (((RU.getNodeByType children (("longCap")[@reason.raw_literal "longCap"]))
        |> unwrap)
       |> parseLongCap)
      |> Location.mknoloc in
  let args =
    RU.getNodesByType children
      (("Expression")[@reason.raw_literal "Expression"])
      (toOcaml.expression toOcaml) in
  let arg =
    match args with
    | [] -> None
    | arg::[] -> ((Some (arg)))
    | _ -> ((Some ((H.Exp.tuple args)))) in
  H.Exp.construct lid arg
let fromConstructor fromOcaml txt maybeValue =
  let first = ("", (fromLongCap txt)) in
  let children =
    match maybeValue with
    | None  -> [first]
    | ((Some
        ({ pexp_desc = ((Pexp_tuple (items))) })))
        -> first ::
        (List.map (emptyLabeled (fromOcaml.fromExpression fromOcaml)) items)
    | ((Some (x))) ->
        [first; ("", (fromOcaml.fromExpression fromOcaml x))] in
  ((("constructor")[@reason.raw_literal "constructor"]), children)
let parseRecord toOcaml children =
  let extends =
    (RU.getNodeByType children
       (("Expression")[@reason.raw_literal "Expression"]))
      |> (optMap (toOcaml.expression toOcaml)) in
  let items =
    RU.getNodesByType children
      (("RecordItem")[@reason.raw_literal "RecordItem"])
      (fun (sub,children,loc,_comments)  ->
         let oloc = ocamlLoc loc in
         let name =
           ((RU.getNodeByType children
               (("longIdent")[@reason.raw_literal "longIdent"]))
              |>
              (unwrapm
                 (("long ident record")[@reason.raw_literal
                                         "long ident record"])))
             |> parseLongIdent in
         let expr =
           (RU.getNodeByType children
              (("Expression")[@reason.raw_literal "Expression"]))
             |> (optMap (toOcaml.expression toOcaml)) in
         let expr =
           match expr with
           | ((Some (x))) -> x
           | None  -> H.Exp.ident (Location.mkloc name oloc) in
         ((Location.mkloc name oloc), expr)) in
  H.Exp.record items extends
let rec parseBaseExpression toOcaml (sub,children,loc,_comments) =
  let oloc = ocamlLoc loc in
  match sub with
  | (("wrapped")[@reason.raw_literal "wrapped"]) ->
      let expr =
        ((RU.getNodeByType children
            (("Expression")[@reason.raw_literal "Expression"]))
           |> unwrap)
          |> (toOcaml.expression toOcaml) in
      (match RU.getNodeByType children (("Type")[@reason.raw_literal "Type"])
       with
       | None  -> expr
       | ((Some (x))) ->
           H.Exp.constraint_ expr (parseType x))
  | (("unexp")[@reason.raw_literal "unexp"]) ->
      let ident =
        (((RU.getContentsByType children
             (("unOp")[@reason.raw_literal "unOp"]))
            |> unwrap)
           |> (stringToIdentLoc oloc))
          |> H.Exp.ident in
      let main =
        ((RU.getNodeByType children
            (("BaseExpression")[@reason.raw_literal "BaseExpression"]))
           |> unwrap)
          |> (parseBaseExpression toOcaml) in
      H.Exp.apply ident [("", main)]
  | (("binop")[@reason.raw_literal "binop"]) ->
      (((RU.getContentsByType children
           (("binOp")[@reason.raw_literal "binOp"]))
          |> (unwrapm (("binOp")[@reason.raw_literal "binOp"])))
         |> (stringToIdentLoc oloc))
        |> H.Exp.ident
  | (("ident")[@reason.raw_literal "ident"]) ->
      let ident =
        ((RU.getNodeByType children
            (("longIdent")[@reason.raw_literal "longIdent"]))
           |> unwrap)
          |> parseLongIdent in
      H.Exp.ident (Location.mkloc ident oloc)
  | (("application")[@reason.raw_literal "application"]) ->
      let base =
        ((RU.getNodeByType children
            (("Expression")[@reason.raw_literal "Expression"]))
           |> unwrap)
          |> (toOcaml.expression toOcaml) in
      let args =
        RU.getNodesByType children (("FnArg")[@reason.raw_literal "FnArg"])
          (parseFnArg toOcaml) in
      H.Exp.apply base args
  | (("const")[@reason.raw_literal "const"]) ->
      H.Exp.constant
        (((RU.getNodeByType children
             (("constant")[@reason.raw_literal "constant"]))
            |> unwrap)
           |> parseConstant)
  | (("tuple")[@reason.raw_literal "tuple"]) ->
      H.Exp.tuple
        (RU.getNodesByType children
           (("Expression")[@reason.raw_literal "Expression"])
           (toOcaml.expression toOcaml))
  | (("funexpr")[@reason.raw_literal "funexpr"]) ->
      ((RU.getNodeByType children
          (("FunExpr")[@reason.raw_literal "FunExpr"]))
         |> unwrap)
        |> (parseFunExpr toOcaml)
  | (("block")[@reason.raw_literal "block"]) ->
      ((RU.getNodeByType children (("Block")[@reason.raw_literal "Block"]))
         |> unwrap)
        |> (parseBlock toOcaml)
  | (("try")[@reason.raw_literal "try"]) ->
      ((RU.getNodeByType children (("TryExp")[@reason.raw_literal "TryExp"]))
         |> unwrap)
        |> (parseTry toOcaml)
  | (("while")[@reason.raw_literal "while"]) ->
      let cond =
        ((RU.getNodeByType children
            (("Expression")[@reason.raw_literal "Expression"]))
           |> unwrap)
          |> (toOcaml.expression toOcaml) in
      let block =
        ((RU.getNodeByType children (("Block")[@reason.raw_literal "Block"]))
           |> unwrap)
          |> (parseBlock toOcaml) in
      H.Exp.while_ cond block
  | (("constructor")[@reason.raw_literal "constructor"]) ->
      parseConstructor toOcaml children
  | (("record")[@reason.raw_literal "record"]) ->
      parseRecord toOcaml children
  | (("if")[@reason.raw_literal "if"]) ->
      ((RU.getNodeByType children (("IfExpr")[@reason.raw_literal "IfExpr"]))
         |> unwrap)
        |> (parseIfExp toOcaml)
  | (("switch")[@reason.raw_literal "switch"]) ->
      ((RU.getNodeByType children
          (("SwitchExp")[@reason.raw_literal "SwitchExp"]))
         |> unwrap)
        |> (parseSwitchExp toOcaml)
  | (("get_attr")[@reason.raw_literal "get_attr"]) ->
      H.Exp.field
        (((RU.getNodeByType children
             (("BaseExpression")[@reason.raw_literal "BaseExpression"]))
            |> unwrap)
           |> (parseBaseExpression toOcaml))
        (Location.mkloc
           (((RU.getNodeByType children
                (("longIdent")[@reason.raw_literal "longIdent"]))
               |> unwrap)
              |> parseLongIdent) oloc)
  | (("unit")[@reason.raw_literal "unit"]) ->
      H.Exp.construct
        (Location.mknoloc
           ((Lident ((("()")[@reason.raw_literal "()"])))))
        None
  | (("list")[@reason.raw_literal "list"]) ->
      H.Exp.array
        (RU.getNodesByType children
           (("Expression")[@reason.raw_literal "Expression"])
           (toOcaml.expression toOcaml))
  | _ ->
      failwith
        ((("not impl - expression - ")[@reason.raw_literal
                                        "not impl - expression - "])
           ^ sub)
let parseBinExpression toOcaml (sub,children,loc,_comments) =
  match sub with
  | (("base")[@reason.raw_literal "base"]) ->
      ((RU.getNodeByType children
          (("BaseExpression")[@reason.raw_literal "BaseExpression"]))
         |> unwrap)
        |> (parseBaseExpression toOcaml)
  | (("binexp")[@reason.raw_literal "binexp"]) ->
      let oloc = ocamlLoc loc in
      let ops =
        RU.getChildren children
          (fun (label,node)  ->
             match (label, node) with
             | ((("op")[@reason.raw_literal "op"]),((Leaf
                (_,contents,_)))) ->
                 ((Some ((contents |> (stringToIdentLoc oloc)))))
             | _ -> None) in
      let items =
        RU.getNodesByType children
          (("BaseExpression")[@reason.raw_literal "BaseExpression"])
          (parseBaseExpression toOcaml) in
      (match items with
       | [] ->
           failwith
             (("no binexp items")[@reason.raw_literal "no binexp items"])
       | exp::rest ->
           let rec loop ops items coll =
             match (ops, items) with
             | ([],[]) -> coll
             | (op::ops,item::items) ->
                 loop ops items
                   (H.Exp.apply (H.Exp.ident op) [("", coll); ("", item)])
             | _ ->
                 failwith
                   (("uneven binops")[@reason.raw_literal "uneven binops"]) in
           loop ops rest exp)
  | _ -> parseBaseExpression toOcaml (sub, children, loc, _comments)
let parseExpression toOcaml (sub,children,loc,_comments) =
  match sub with
  | (("base")[@reason.raw_literal "base"]) ->
      ((RU.getNodeByType children
          (("BaseExpression")[@reason.raw_literal "BaseExpression"]))
         |> unwrap)
        |> (parseBaseExpression toOcaml)
  | (("binary")[@reason.raw_literal "binary"]) ->
      ((RU.getNodeByType children
          (("BinExpression")[@reason.raw_literal "BinExpression"]))
         |> unwrap)
        |> (parseBinExpression toOcaml)
  | (("ternary")[@reason.raw_literal "ternary"]) ->
      let condition =
        (((RU.getNodeByLabel children
             (("condition")[@reason.raw_literal "condition"]))
            |> (unwrapm (("condition")[@reason.raw_literal "condition"])))
           |> stripRuleName)
          |> (parseBinExpression toOcaml) in
      let consequent =
        (((RU.getNodeByLabel children
             (("consequent")[@reason.raw_literal "consequent"]))
            |> (unwrapm (("consequent")[@reason.raw_literal "consequent"])))
           |> stripRuleName)
          |> (parseBinExpression toOcaml) in
      let alternate =
        (((RU.getNodeByLabel children
             (("alternate")[@reason.raw_literal "alternate"]))
            |> (unwrapm (("alternate")[@reason.raw_literal "alternate"])))
           |> stripRuleName)
          |> (parseBinExpression toOcaml) in
      H.Exp.ifthenelse
        ~attrs:[((Location.mknoloc
                    (("ternary")[@reason.raw_literal "ternary"])),
                  ((PStr ([]))))] condition consequent
        ((Some (alternate)))
  | _ -> parseBinExpression toOcaml (sub, children, loc, _comments)
let rec unwrapList fromOcaml ({ pexp_desc;_} as expression) =
  match pexp_desc with
  | ((Pexp_construct
      ({
         txt = ((Lident
           ((("[]")[@reason.raw_literal "[]"]))));_},None
       )))
      -> []
  | ((Pexp_construct
      ({
         txt = ((Lident
           ((("::")[@reason.raw_literal "::"]))));_},((Some
       ({ pexp_desc = ((Pexp_tuple (first::second::[])));_}))
       ))))
      -> (fromOcaml.fromExpression fromOcaml first) ::
      (unwrapList fromOcaml second)
  | _ -> [fromOcaml.fromExpression fromOcaml expression]
let opChars =
  (("!?~$%&*+-./:<=>@^|")[@reason.raw_literal "!?~$%&*+-./:<=>@^|"])
let binOpChars = (("$%&*+-./:<=>@^|")[@reason.raw_literal "$%&*+-./:<=>@^|"])
let unOpChars = (("!?~")[@reason.raw_literal "!?~"])
let startsWith chars txt = String.contains chars (txt.[0])
let wrapBinExp (sub,children) =
  ((("base")[@reason.raw_literal "base"]),
    [("",
       (node
          ((("BaseExpression")[@reason.raw_literal "BaseExpression"]),
            (("wrapped")[@reason.raw_literal "wrapped"]))
          [("",
             (node
                ((("Expression")[@reason.raw_literal "Expression"]),
                  (("binary")[@reason.raw_literal "binary"]))
                [("",
                   (node
                      ((("BinExpression")[@reason.raw_literal
                                           "BinExpression"]), sub) children
                      mLoc))] mLoc))] mLoc))])
let wrapBaseExp (sub,children) =
  ((("wrapped")[@reason.raw_literal "wrapped"]),
    [("",
       (node
          ((("Expression")[@reason.raw_literal "Expression"]),
            (("base")[@reason.raw_literal "base"]))
          [("",
             (node
                ((("BaseExpression")[@reason.raw_literal "BaseExpression"]),
                  sub) children mLoc))] mLoc))])
let wrapExp (sub,children) =
  ((("wrapped")[@reason.raw_literal "wrapped"]),
    [("",
       (node ((("Expression")[@reason.raw_literal "Expression"]), sub)
          children mLoc))])
let fromRecord fromOcaml items extends =
  let exp = extends |> (optMap (fromOcaml.fromExpression fromOcaml)) in
  let args =
    List.map
      (emptyLabeled
         (fun (ident,exp)  ->
            let children =
              match exp.pexp_desc with
              | ((Pexp_ident ({ txt = name;_}))) when
                  name = ident.txt -> [("", (fromLongIdent ident.txt))]
              | _ ->
                  [("", (fromLongIdent ident.txt));
                  ("", (fromOcaml.fromExpression fromOcaml exp))] in
            node ((("RecordItem")[@reason.raw_literal "RecordItem"]), "")
              children mLoc)) items in
  let children =
    match exp with
    | ((Some (x))) -> ("", x) :: args
    | None  -> args in
  ((("record")[@reason.raw_literal "record"]), children)
let rec fromBaseExpression fromOcaml
  ({ pexp_desc; pexp_attributes;_} as expression) =
  let (sub,children) =
    match pexp_desc with
    | ((Pexp_ident ({ txt;_}))) ->
        (match txt with
         | ((Lident (txt))) when startsWith opChars txt ->
             ((("binop")[@reason.raw_literal "binop"]),
               [("",
                  ((Leaf
                      (((("binOp")[@reason.raw_literal "binOp"]), ""), txt,
                        mLoc))))])
         | _ ->
             ((("ident")[@reason.raw_literal "ident"]),
               [("", (fromLongIdent txt))]))
    | ((Pexp_constant (constant))) ->
        ((("const")[@reason.raw_literal "const"]),
          [("",
             ((fromConstant constant) |>
                (nodeWrap (("constant")[@reason.raw_literal "constant"]))))])
    | ((Pexp_fun (label,maybeDefault,pattern,expr))) ->
        ((("funexpr")[@reason.raw_literal "funexpr"]),
          [("", (fromFunExpr fromOcaml label maybeDefault pattern expr))])
    | ((Pexp_apply (base,args))) ->
        (match (base, args) with
         | ({
              pexp_desc = ((Pexp_ident
                ({ txt = ((Lident (txt)));_})))
              },("",arg)::[])
             when
             (startsWith unOpChars txt) ||
               ((txt = (("-")[@reason.raw_literal "-"])) ||
                  (txt = (("-.")[@reason.raw_literal "-."])))
             ->
             ((("unexp")[@reason.raw_literal "unexp"]),
               [("",
                  ((Leaf
                      (((("unOp")[@reason.raw_literal "unOp"]), ""), txt,
                        mLoc))));
               ("", (fromBaseExpression fromOcaml arg))]) |> wrapBaseExp
         | _ ->
             ((("application")[@reason.raw_literal "application"]),
               (("", (fromOcaml.fromExpression fromOcaml base)) ::
               (List.map (emptyLabeled (fromFnArg fromOcaml)) args))))
    | ((Pexp_record (items,extends))) ->
        fromRecord fromOcaml items extends
    | ((Pexp_let (isRec,values,exp))) ->
        let children = (fromLet fromOcaml isRec values) ::
          (unwrapSequence fromOcaml exp) in
        ((("block")[@reason.raw_literal "block"]),
          [("",
             (node ((("Block")[@reason.raw_literal "Block"]), "")
                (List.map withEmptyLabels children) mLoc))])
    | ((Pexp_sequence (first,second))) ->
        let children =
          List.concat
            [unwrapSequence fromOcaml first; unwrapSequence fromOcaml second] in
        ((("block")[@reason.raw_literal "block"]),
          [("",
             (node ((("Block")[@reason.raw_literal "Block"]), "")
                (List.map withEmptyLabels children) mLoc))])
    | ((Pexp_tuple (items))) ->
        ((("tuple")[@reason.raw_literal "tuple"]),
          (List.map (emptyLabeled (fromOcaml.fromExpression fromOcaml)) items))
    | ((Pexp_field (expr,{ txt;_}))) ->
        ((("get_attr")[@reason.raw_literal "get_attr"]),
          [("", (fromBaseExpression fromOcaml expr));
          ("", (fromLongIdent txt))])
    | ((Pexp_construct
        ({
           txt = ((Lident
             ((("[]")[@reason.raw_literal "[]"]))));_},None
         )))
        -> ((("list")[@reason.raw_literal "list"]), [])
    | ((Pexp_construct
        ({
           txt = ((Lident
             ((("::")[@reason.raw_literal "::"]))));_},((Some
         ({
            pexp_desc = ((Pexp_tuple (first::second::[])));_}))
         ))))
        ->
        ((("list")[@reason.raw_literal "list"]),
          (("", (fromOcaml.fromExpression fromOcaml first)) ::
          (List.map withEmptyLabels (unwrapList fromOcaml second))))
    | ((Pexp_construct ({ txt;_},maybeValue))) ->
        fromConstructor fromOcaml txt maybeValue
    | ((Pexp_try (base,cases))) ->
        fromTry fromOcaml base cases
    | ((Pexp_match (base,cases))) ->
        fromSwitchExp fromOcaml base cases
    | ((Pexp_while (cond,block))) ->
        let block = block |> (fromBlock fromOcaml) in
        let cond = cond |> (fromOcaml.fromExpression fromOcaml) in
        ((("while")[@reason.raw_literal "while"]), [("", cond); ("", block)])
    | ((Pexp_ifthenelse (condition,consequent,maybeAlt)))
        ->
        (match (pexp_attributes, maybeAlt) with
         | (({ txt = (("ternary")[@reason.raw_literal "ternary"]);_},_)::[],((Some
            (alternate)))) ->
             ((("ternary")[@reason.raw_literal "ternary"]),
               [((("condition")[@reason.raw_literal "condition"]),
                  (fromBinExpression fromOcaml condition));
               ((("consequent")[@reason.raw_literal "consequent"]),
                 (fromBinExpression fromOcaml consequent));
               ((("alternate")[@reason.raw_literal "alternate"]),
                 (fromBinExpression fromOcaml alternate))])
               |> wrapExp
         | _ -> fromIfExp fromOcaml (condition, consequent, maybeAlt))
    | _ ->
        (Printast.expression 0 Format.std_formatter expression;
         failwith (("no exp")[@reason.raw_literal "no exp"])) in
  node ((("BaseExpression")[@reason.raw_literal "BaseExpression"]), sub)
    children mLoc
and fromBinExp fromOcaml op left right =
  [("", (fromBaseExpression fromOcaml left));
  ((("op")[@reason.raw_literal "op"]),
    ((Leaf (((("binOp")[@reason.raw_literal "binOp"]), ""), op, mLoc))
    ));
  ("", (fromBaseExpression fromOcaml right))]
and fromBinExpression fromOcaml ({ pexp_desc;_} as expression) =
  let (sub,children) =
    match pexp_desc with
    | ((Pexp_apply
        ({
           pexp_desc = ((Pexp_ident
             ({ txt = ((Lident (txt)));_})))
           },args)))
        when
        (startsWith binOpChars txt) ||
          ((txt = (("or")[@reason.raw_literal "or"])) ||
             (txt = (("mod")[@reason.raw_literal "mod"])))
        ->
        (match args with
         | ("",left)::("",right)::[] ->
             ((("binexp")[@reason.raw_literal "binexp"]),
               (fromBinExp fromOcaml txt left right)) |> wrapBinExp
         | _ ->
             ((("base")[@reason.raw_literal "base"]),
               [("", (fromBaseExpression fromOcaml expression))]))
    | _ ->
        ((("base")[@reason.raw_literal "base"]),
          [("", (fromBaseExpression fromOcaml expression))]) in
  node ((("BinExpression")[@reason.raw_literal "BinExpression"]), sub)
    children mLoc
let fromExpression fromOcaml ({ pexp_desc; pexp_attributes;_} as expression)
  =
  let (sub,children) =
    match (pexp_attributes, pexp_desc) with
    | (({ txt = (("ternary")[@reason.raw_literal "ternary"]);_},_)::[],((Pexp_ifthenelse
       (condition,consequent,((Some (alternate)))))
       )) ->
        ((("ternary")[@reason.raw_literal "ternary"]),
          [((("condition")[@reason.raw_literal "condition"]),
             (fromBinExpression fromOcaml condition));
          ((("consequent")[@reason.raw_literal "consequent"]),
            (fromBinExpression fromOcaml consequent));
          ((("alternate")[@reason.raw_literal "alternate"]),
            (fromBinExpression fromOcaml alternate))])
    | _ ->
        ((("binary")[@reason.raw_literal "binary"]),
          [("", (fromBinExpression fromOcaml expression))]) in
  node ((("Expression")[@reason.raw_literal "Expression"]), sub) children
    mLoc
let toOcaml = { structure = parseStructure; expression = parseExpression }
let fromOcaml = { fromStructure; fromExpression }
let convert result =
  match result with
  | ((Node
      (((("Start")[@reason.raw_literal "Start"]),_),children,_,_)))
      ->
      RU.getNodesByType children
        (("Structure")[@reason.raw_literal "Structure"])
        (toOcaml.structure toOcaml)
  | _ -> failwith ""
let convertFrom structures =
  node ((("Start")[@reason.raw_literal "Start"]), "")
    (List.map (labeled "" (fromOcaml.fromStructure fromOcaml)) structures)
    Location.none
end
module PrettyOutput
= struct
#1 "PrettyOutput.ml"
[@@@ocaml.ppx.context { cookies = [] }]
module Output =
  struct
    type outputT =
      | Text of string
      | EOL
      | NoSpace
      | MaybeNewlined of outputT list
      | Newlined of outputT list
      | Lexical of outputT list
      | Straight of outputT list
  end
type config = {
  maxWidth: int;
  indentWidth: int;
  indentStr: string;}
let pad num base =
  let txt = ref "" in for i = 0 to num do txt := ((!txt) ^ base) done; !txt
type iterim =
  | Text of string
  | NoSpace
let rec outputToString config indentLevel output =
  match output with
  | Output.EOL  ->
      (((("\n")[@reason.raw_literal "\\n"]) ^
          (pad (indentLevel - 1) config.indentStr)), true)
  | Output.NoSpace  ->
      failwith
        (("unhandled nospace")[@reason.raw_literal "unhandled nospace"])
  | ((Output.Text (str))) -> (str, false)
  | ((Output.Lexical (items))) ->
      ((String.concat ""
          (List.map (fun x  -> fst (outputToString config 0 x)) items)),
        false)
  | ((Output.Straight (items))) ->
      let rec loop items =
        match items with
        | [] -> ("", false)
        | (Output.NoSpace )::rest -> loop rest
        | child::[] -> outputToString config indentLevel child
        | child::(Output.NoSpace )::rest ->
            let (restext,multi) = loop rest in
            let (res,nmulti) = outputToString config indentLevel child in
            ((res ^ restext), (multi || nmulti))
        | child::rest ->
            let (restext,multi) = loop rest in
            let (res,nmulti) = outputToString config indentLevel child in
            ((res ^ (((" ")[@reason.raw_literal " "]) ^ restext)),
              (multi || nmulti)) in
      loop items
  | ((Output.MaybeNewlined (items))) ->
      let rec loop items =
        match items with
        | [] -> ([], 0, 0)
        | (Output.NoSpace )::rest ->
            let (items,len,multis) = loop rest in
            ((NoSpace :: items), len, multis)
        | child::rest ->
            let (items,len,multis) = loop rest in
            let (res,nmulti) = outputToString config (indentLevel + 1) child in
            ((((Text (res))) :: items),
              (len + (String.length res)),
              ((match nmulti with | true  -> multis + 1 | false  -> multis))) in
      let (items,total,multis) = loop items in
      if
        (multis > 1) ||
          (((total + (config.indentWidth * indentLevel)) > config.maxWidth)
             && ((List.length items) > 1))
      then
        let padt =
          (("\n")[@reason.raw_literal "\\n"]) ^
            (pad (indentLevel + 0) config.indentStr) in
        let rec loop items =
          match items with
          | [] -> ""
          | ((Text (child)))::[] -> child
          | ((Text (child)))::(NoSpace )::rest ->
              child ^ (loop rest)
          | ((Text (child)))::rest ->
              child ^ (padt ^ (loop rest))
          | (NoSpace )::rest -> loop rest in
        ((padt ^
            ((loop items) ^
               ((("\n")[@reason.raw_literal "\\n"]) ^
                  (pad (indentLevel - 1) config.indentStr)))), true)
      else
        (let rec loop items =
           match items with
           | [] -> ""
           | ((Text (child)))::[] -> child
           | ((Text (child)))::(NoSpace )::rest ->
               child ^ (loop rest)
           | ((Text (child)))::rest ->
               child ^ (((" ")[@reason.raw_literal " "]) ^ (loop rest))
           | (NoSpace )::rest -> loop rest in
         ((loop items), false))
  | ((Output.Newlined (items))) ->
      let padt =
        (("\n")[@reason.raw_literal "\\n"]) ^
          (pad (indentLevel + 0) config.indentStr) in
      let rec loop items =
        match items with
        | [] -> ("", false)
        | (Output.NoSpace )::rest -> loop rest
        | child::[] -> outputToString config (indentLevel + 1) child
        | child::(Output.NoSpace )::rest ->
            let (restext,multi) = loop rest in
            let (res,nmulti) = outputToString config (indentLevel + 1) child in
            ((res ^ restext), (multi || nmulti))
        | child::rest ->
            let (restext,multi) = loop rest in
            let (res,nmulti) = outputToString config (indentLevel + 1) child in
            ((res ^ (padt ^ restext)), (multi || nmulti)) in
      let (str,multi) = loop items in
      ((padt ^
          (str ^
             ((("\n")[@reason.raw_literal "\\n"]) ^
                (pad (indentLevel - 1) config.indentStr)))), multi)
end
