
external setupComm: ('a => unit) => ('b => unit) = "setupComm" [@@bs.val];

let onMessage : (BrowserTypes.fromMain => unit) = fun message => {

};

let sendMessage : (BrowserTypes.fromWorker => unit) = setupComm onMessage;

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
