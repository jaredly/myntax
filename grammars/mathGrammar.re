
module G = {
  [@name "Start"]
  [%%rule (
    "Expression",
    ([@node "Expression"]expr) => expr
  )];

  [@name "Expression"]
  [%%rules [(
    "add",
    {|[left]Expression "+" [right]Expression|},
    ([@node.left "Expression"]left, [@node.right "Expression"]right) => left + right
  ), (
    "sub",
    {|[left]Expression "-" [right]Expression|},
    ([@node.left "Expression"]left, [@node.right "Expression"]right) => left - right
  ), (
    "int", {|int|},
    ([@text "int"](int, _)) => int_of_string(int)
  )]];

  [@name "int"] [@leaf] [%%rule "digit+"];
  [@name "digit"] [%%rule "'0'..'9'"];
};

let calc = text => {
  let result = Grammar.getResult(LispGrammar.grammar, "Start", text);
  G.convert_Start(result);
};
