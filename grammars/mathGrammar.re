
module DSL = PackTypes.DSL;

[@name "Start"]
[%%rule (
  DSL.[n("Expression")],
  ([@node "Expression"]expr) => expr
)];

[@name "Expression"]
[%%rules [(
  "add", DSL.[n(~label="left", "Expression"), t("+"), n(~label="right", "Expression")],
  ([@node.left "Expression"]left, [@node.right "Expression"]right) => left + right
), (
  "sub", DSL.[n(~label="left", "Expression"), t("-"), n(~label="right", "Expression")],
  ([@node.left "Expression"]left, [@node.right "Expression"]right) => left - right
), (
  "int", DSL.[n("int")],
  ([@text "int"](int, _)) => int_of_string(int)
)]];

[@name "int"]
[@leaf]
[%%rule DSL.[plus(n("digit"))]];

[@name "digit"]
[%%rule DSL.[chars('0', '9')]]
