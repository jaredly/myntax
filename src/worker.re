
/* external setupComm: ('a => unit) => ('b => unit) = "setupComm" [@@bs.val];

let grammarText = ref "";
let inputText = ref "";
let grammar = ref None;
let input = ref None;

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

let onMessage : (BrowserTypes.fromMain => unit) = fun (newGrammar, newInput) => {
  if (newGrammar != !grammarText) {
    grammarText := newGrammar;
    switch (parseGrammar newGrammar) {
      | Some made => {
        grammar := Some made;
      }
      | None => ()
    }
  }
};

let sendMessage : (BrowserTypes.fromWorker => unit) = setupComm onMessage; */
