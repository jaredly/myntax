
[@name "Start"]
[%%rule (
  "Expression",
  ([@node "Expression"]expr) => expr
)];

[@name "Expression"]
[%%rule [(
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
[@name "digit"] [%%rule "'0..9'"];
