[%%bs.raw
  {|
var setupComm = function(fn) {
  onmessage = evt => fn(evt.data);
  return data => postMessage(data);
};
|}
];

type comm('b) = 'b => unit;

[@bs.val] external setupComm : ('a => unit) => comm('b) = "setupComm";

[@bs.val] external perfNow : unit => float = "performance.now";

let grammarText = ref("");

let inputText = ref("");

let grammar = ref(None);

let sendMessage: ref((BrowserTypes.fromWorker => unit)) = ref((x) => ());

let inputResult = ref(None);

let rec parseGrammar = (text, sendMessage) => {
  let start = perfNow();
  switch (Runtime.parse(GrammarGrammar.grammar, "Start", text)) {
  | PackTypes.Result.Success(result) =>
    let mid = perfNow();
    let newGrammar = GrammarOfGrammar.convert(result);
    sendMessage(BrowserTypes.GrammarGood(mid -. start, perfNow() -. mid));
    Some(newGrammar)
  | PackTypes.Result.Failure(maybeResult, partial) =>
    sendMessage(BrowserTypes.GrammarBad(partial)); /*** TODO send over result? */
    None
  }
};

let parseInput = (fromGrammar, text, grammar, sendMessage) => {
  let start = perfNow();
  switch (Runtime.parse(grammar, "Start", text)) {
  | PackTypes.Result.Success(result) =>
    sendMessage(BrowserTypes.InputGood(result, perfNow() -. start));
    inputResult := Some(result)
  /* Some result */
  | PackTypes.Result.Failure(maybeResult, partial) =>
    inputResult := None;
    sendMessage(BrowserTypes.InputBad(partial)) /*** TODO send over result? */
  /* None */
  }
};

let onMessage: BrowserTypes.fromMain => unit =
  (message) =>
    switch message {
    | Change((newGrammar, newInput)) =>
      if (newGrammar != grammarText^) {
        grammarText := newGrammar;
        switch (parseGrammar(newGrammar, sendMessage^)) {
        | Some(made) =>
          grammar := Some(made);
          switch inputResult^ {
          | None => parseInput(true, newInput, made, sendMessage^)
          | Some(result) =>
            switch (PrettyPrint.toString(made, result)) {
            | Some(x) => sendMessage^(BrowserTypes.InputPretty(x, 0.0))
            | None => ()
            }
          }
        | None => ()
        }
      } else if (newInput != inputText^) {
        inputText := newInput;
        switch grammar^ {
        | Some(grammar) =>
          parseInput(false, newInput, grammar, sendMessage^);
          ()
        | None => ()
        }
      }
    | Refmt =>
      switch (grammar^, inputResult^) {
      | (Some(grammar), Some(result)) =>
        switch (PrettyPrint.toString(grammar, result)) {
        | Some(x) => sendMessage^(BrowserTypes.InputPretty(x, 0.0))
        | None => ()
        }
      | _ => ()
      }
    };

sendMessage := setupComm(onMessage);
