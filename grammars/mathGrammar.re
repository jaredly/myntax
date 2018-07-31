
module DSL = PackTypes.DSL;

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

[@name "int"]
[@leaf]
[%%rule DSL.[plus(n("digit"))]];

[@name "digit"]
[%%rule DSL.[chars('0', '9')]]
