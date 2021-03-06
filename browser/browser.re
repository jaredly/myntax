/* This file is compiled by bucklescript, bundled by webpack, and served up w/ static/index.html */
type node;

[@bs.val] external getElementById : string => node = "document.getElementById";

[@bs.send] external addEventListener : (node, string, unit => unit) => unit = "addEventListener";

[@bs.val] external getA : ('a, string) => 'b = "window.getA";

[@bs.val] external getOpt : ('a, string) => Js.Undefined.t('b) = "window.getA";

[@bs.val] external setA : ('a, string, 'b) => unit = "window.setA";

type timeout = int;

[@bs.val] external setTimeout : (unit => unit, int) => timeout = "window.setTimeout";

[@bs.val] external clearTimeout : timeout => unit = "window.clearTimeout";

type ls;

[@bs.val] external localStorage : ls = "window.localStorage";

[@bs.val] external onGrammar : (string => unit) => unit = "window.onGrammar";

[@bs.val] external onInput : (string => unit) => unit = "window.onInput";

[@bs.val] external onRefmt : (unit => unit) => unit = "window.onRefmt";

[@bs.val] external now : unit => int = "Date.now";

/* workers */
type resFn('b) = 'b => unit;

[@bs.val] external setupWorker : (string, 'a => unit) => resFn('b) = "window.setupWorker";

let getValue = (node) => getA(node, "value");

let setValue = (node, text) => setA(node, "value", text);

let grammarEl = getElementById("grammar");

let inputEl = getElementById("input");

let grammarStatus = getElementById("grammar-status");

let inputStatus = getElementById("input-status");

let setText = (node, text) => setA(node, "textContent", text);

let debounce = (fn, time) => {
  let tout = ref(0);
  (arg) => {
    clearTimeout(tout^);
    tout := setTimeout(() => fn(arg), time)
  }
};

let defaultGrammar = {|Start = "fun" Patt "->" Expr "\n"*

Exprs = (Expr #"\n")* Expr?

Expr =
  | constant -- const
  | "fun" Patt "->" Expr -- fun ; function
  | "(" [contents]Expr ")" -- parens
  | [left]Expr "+" [right]Expr -- add

Patt = ident

constant =
  | ident -- ident
  | int64 -- int

@leaf
ident = alpha+
@leaf
int64 =  digit+

reserved =
  | "fun"
  | "let"

alpha = 'a..z'
digit = '0..9'
|};

let getLS = (name) => Js.Undefined.to_opt(getOpt(localStorage, name));

let rawGrammar =
  ref(
    switch (getLS("grammar")) {
    | Some("") => defaultGrammar
    | Some(text) => text
    | None => defaultGrammar
    }
  );

let defaultInput = "fun x -> 10";

let rawInput =
  ref(
    switch (getLS("input")) {
    | Some("") => defaultInput
    | Some(text) => text
    | None => defaultInput
    }
  );

setValue(grammarEl, rawGrammar^);

setValue(inputEl, rawInput^);

let unwrap = ResultUtils.unwrap;

let result = ref(None);

let bounce = 200;

let waiting = ref(None);

let sendIt = ref((x) => ());

let onMessage: BrowserTypes.fromWorker => unit =
  (x) => {
    switch x {
    | BrowserTypes.GrammarGood(parse, convert) =>
      setText(
        grammarStatus,
        Printf.sprintf("Grammar parsed in %f and converted in %f", parse, convert)
      )
    | BrowserTypes.GrammarBad(partial) => setText(grammarStatus, "Grammar failed to parse")
    | BrowserTypes.InputPretty(newInput, time) =>
      print_endline("New pretty! " ++ newInput);
      if (newInput != rawInput^) {
        setValue(inputEl, newInput);
        rawInput := newInput
      }
    | BrowserTypes.InputGood(res, parse) =>
      setText(inputStatus, Printf.sprintf("Input parsed in %f", parse));
      result := Some(res)
    | BrowserTypes.InputBad(partial) => setText(inputStatus, "Input failed to parse")
    };
    switch waiting^ {
    | None => ()
    | Some(x) => sendIt^(x)
    }
  };

let sendMessage: BrowserTypes.fromMain => unit = setupWorker("./worker.js", onMessage);

sendIt := sendMessage;

let maybeSend = (message) =>
  switch waiting^ {
  | None =>
    waiting := None;
    sendIt^(message)
  | Some(x) => waiting := Some(message)
  };

onGrammar(
  debounce(
    (text) =>
      if (text != rawGrammar^) {
        rawGrammar := text;
        setA(localStorage, "grammar", text);
        maybeSend(Change((text, rawInput^)))
      },
    100
  )
);

onInput(
  debounce(
    (text) =>
      if (text != rawInput^) {
        rawInput := text;
        setA(localStorage, "input", text);
        maybeSend(Change((rawGrammar^, text)))
      },
    100
  )
);

onRefmt(() => maybeSend(Refmt));

maybeSend(Change((rawGrammar^, rawInput^)));
