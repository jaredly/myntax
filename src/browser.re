/* This file is compiled by bucklescript, bundled by webpack, and served up w/ static/index.html */

type node;
external getElementById : string => node = "document.getElementById" [@@bs.val];
external addEventListener : node => string => (unit => unit) => unit = "addEventListener" [@@bs.send];
external getA : 'a => string => 'b = "window.getA" [@@bs.val];
external getOpt : 'a => string => Js.Undefined.t 'b = "window.getA" [@@bs.val];
external setA : 'a => string => 'b => unit = "window.setA" [@@bs.val];
type timeout = int;
external setTimeout : (unit => unit) => int => timeout = "window.setTimeout" [@@bs.val];
external clearTimeout : timeout => unit = "window.clearTimeout" [@@bs.val];
type ls;
external localStorage : ls = "window.localStorage" [@@bs.val];

external onGrammar : (string => unit) => unit = "window.onGrammar" [@@bs.val];
external onInput : (string => unit) => unit = "window.onInput" [@@bs.val];
external now : unit => int = "Date.now" [@@bs.val];

let getValue node => getA node "value";
let setValue node text => setA node "value" text;

let grammarEl = getElementById "grammar";
let inputEl = getElementById "input";
let grammarStatus = getElementById "grammar-status";
let inputStatus = getElementById "input-status";

let setText node text => setA node "textContent" text;

let debounce fn time => {
  let tout = ref 0;
  fun arg => {
    clearTimeout !tout;
    tout := setTimeout (fun () => {
      fn arg;
    }) time;
  }
};

let defaultGrammar = {|Start = "fun" Patt "->" Expr "\n"*

Exprs = (Expr #"\n")* Expr?

Expr =
  | constant
  | "fun" Patt "->" Expr -- fun ; function
  | "(" [contents]Expr ")" -- parens
  | [left]Expr "+" [right]Expr -- add

Patt = ident

constant =
  | ident
  | int64

ident = alpha+
int64 =  digit+

reserved =
  | "fun"
  | "let"

alpha = 'a..z'
digit = '0..9'
|};

let getLS name => {
  Js.Undefined.to_opt (getOpt localStorage name)
};

let rawGrammar = ref (switch (getLS "grammar") {
  | Some text => text
  | None => defaultGrammar
});

let defaultInput = "fun x -> 10";

let rawInput = ref (switch (getLS "input") {
  | Some text => text
  | None => defaultInput
});

setValue grammarEl !rawGrammar;
setValue inputEl !rawInput;


let parseGrammar text => {
  switch (Runtime.parse GrammarGrammar.grammar "Start" text) {
    | Runtime.Complete result => {
      setText grammarStatus "Parsed grammar";
      Some (GrammarOfGrammar.convert result)
    }
    | _ => {
      setText grammarStatus "Failed to parse!";
      None
    }
  }
};

let parseInput text grammar => {
  switch (Runtime.parse grammar "Start" text) {
    | Runtime.Complete result => {
      setText inputStatus "Parsed input";
      Some result
    }
    | _ => {
      setText inputStatus "Failed to parse input";
      None
    }
  }
};

let unwrap = GrammarOfGrammar.unwrap;

let grammar = ref (parseGrammar !rawGrammar);
let result = ref (switch !grammar {
  | Some g => (parseInput !rawInput g)
  | None => None
});

let bounce = 200;

onGrammar (debounce (fun text => {
  if (text != !rawGrammar)  {
    rawGrammar := text;
    setA localStorage "grammar" text;
    let newG = (parseGrammar text);
    grammar := newG;
    switch newG {
      | Some newG => {
        result := parseInput text newG;
      }
      | None => ()
    }
  }
}) bounce);

onInput (debounce (fun text => {
  if (text != !rawInput) {
    rawInput := text;
    setA localStorage "input" text;
    switch (!grammar) {
      | Some g => {
        result := (parseInput text g)
      }
      | None => ()
    }
  }
}) bounce);
