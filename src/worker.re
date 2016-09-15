
[%%bs.raw {|
var setupComm = function(fn) {
  onmessage = evt => fn(evt.data);
  return data => postMessage(data);
};
|}];

type comm 'b = 'b => unit;
external setupComm: ('a => unit) => comm 'b = "setupComm" [@@bs.val];
external perfNow: unit => float = "performance.now" [@@bs.val];

let grammarText = ref "";
let inputText = ref "";
let grammar = ref None;
let sendMessage : ref (BrowserTypes.fromWorker => unit) = ref (fun x => ());
/* let input = ref None; */

let rec parseGrammar text sendMessage => {
  let start = perfNow();
  switch (Runtime.parse GrammarGrammar.grammar "Start" text) {
    | PackTypes.Result.Success result => {
      let mid = perfNow();
      let newGrammar = GrammarOfGrammar.convert result;
      sendMessage (BrowserTypes.GrammarGood (mid -. start) (perfNow() -. mid));
      Some newGrammar
    }
    | PackTypes.Result.Failure maybeResult partial => {
      sendMessage (BrowserTypes.GrammarBad partial); /** TODO send over result? */
      None
    }
  }
};

let parseInput text grammar sendMessage => {
  let start = perfNow();
  switch (Runtime.parse grammar "Start" text) {
    | PackTypes.Result.Success result => {
      sendMessage (BrowserTypes.InputGood result (perfNow() -. start));
      /* Some result */
    }
    | PackTypes.Result.Failure maybeResult partial => {
      sendMessage (BrowserTypes.InputBad partial); /** TODO send over result? */
      /* None */
    }
  }
};

let onMessage : (BrowserTypes.fromMain => unit) = fun (newGrammar, newInput) => {
  if (newGrammar != !grammarText) {
    grammarText := newGrammar;
    switch (parseGrammar newGrammar !sendMessage) {
      | Some made => {
        grammar := Some made;
        parseInput newInput made !sendMessage;
      }
      | None => ()
    };
  } else if (newInput != !inputText) {
    inputText := newInput;
    switch (!grammar) {
      | Some grammar => {
        parseInput newInput grammar !sendMessage;
        ()
      }
      | None => ()
    };
  };
};

sendMessage := setupComm onMessage;
