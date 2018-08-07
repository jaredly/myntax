open PackTypes.Result;

module P = PackTypes.Parsing;

let rec getChild = (children, mapper) =>
  switch children {
  | [] => None
  | [child, ...rest] =>
    switch (mapper(child)) {
    | None => getChild(rest, mapper)
    | x => x
    }
  };

let rec getChildren = (children, mapper) =>
  switch children {
  | [] => []
  | [child, ...rest] =>
    switch (mapper(child)) {
    | None => getChildren(rest, mapper)
    | Some(x) => [x, ...getChildren(rest, mapper)]
    }
  };

let getContentsByLabel = (children, needle) =>
  getChild(
    children,
    ((label, child)) =>
      if (needle != label) {
        None
      } else {
        switch child {
        | Leaf(_, contents, _) => Some(contents)
        | Comment(_) | Node(_) => failwith("expected a leaf")
        }
      }
  );

let getManyContentsByType = (children, needle) =>
  getChildren(
    children,
    (child) =>
      switch child {
      | (_, Leaf((name, _), contents, _)) when name == needle => Some(contents)
      | (_, Node((name, _), _, _, _)) when name == needle => failwith("expected a leaf")
      | _ => None
      }
  );

let getContentsByType = (children, needle) =>
  getChild(
    children,
    (child) =>
      switch child {
      | (_, Leaf((name, _), contents, _)) when name == needle => Some(contents)
      | (_, Node((name, _), _, _, _)) when name == needle => failwith("expected a leaf")
      | _ => None
      }
  );

let rec getPresence = (children, mapper) =>
  switch children {
  | [] => false
  | [child, ...rest] =>
    switch (mapper(child)) {
    | false => getPresence(rest, mapper)
    | x => x
    }
  };

let getPresenceByLabel = (children, needle) =>
  getPresence(
    children,
    (child) =>
      switch child {
      | (name, _) when name == needle => true
      | _ => false
      }
  );

let getPresenceByType = (children, needle) =>
  getPresence(
    children,
    (child) =>
      switch child {
      | (_, Leaf((name, _), _, _))
      | (_, Node((name, _), _, _, _)) when name == needle => true
      | _ => false
      }
  );

let getNodeByType = (children, needle) =>
  getChild(
    children,
    ((label, child)) =>
      if (label != "") {
        None
      } else {
        switch child {
        | Node((name, sub), children, loc, comments) when name == needle => Some((sub, children, loc, comments))
        | _ => None
        }
      }
  );

let getNodesByType = (children, needle, nodeMapper) =>
  getChildren(
    children,
    ((label, child)) =>
      switch child {
      | Node((name, sub), children, loc, comments) when name == needle =>
        Some(nodeMapper((sub, children, loc, comments)))
      | _ => None
      }
  );

let getNodesByLabel = (children, needle, nodeMapper) =>
  getChildren(
    children,
    ((label, child)) => {
      if (label == needle) {
        switch child {
        | Node((name, sub), children, loc, comments) => Some(nodeMapper((sub, children, loc, comments)))
        | _ => None
        }
      } else {
        None
      }
    }
  );

let getNodeByLabel = (children, needle) =>
  getChild(
    children,
    ((label, child)) =>
      if (label == needle) {
        switch child {
        | Node(rule, children, loc, comments) => Some((rule, children, loc, comments))
        | Comment(_) | Leaf(_) => failwith("Expected node for label " ++ needle)
        }
      } else {
        None
      }
  );

let getLeafsByType = (children, needle) =>
  getChildren(
    children,
    ((label, child)) =>
      switch child {
      | Leaf((name, sub), contents, loc) when name == needle => Some((contents, loc))
      | _ => None
      }
  );

let getLeafByType = (children, needle) =>
  getChild(
    children,
    ((label, child)) =>
      switch child {
      | Leaf((name, sub), contents, loc) when name == needle => Some((contents, loc))
      | _ => None
      }
  );

let getLeafByLabel = (children, needle) =>
  getChild(
    children,
    ((label, child)) =>
      switch child {
      | Leaf((name, sub), contents, loc) when label == needle => Some((contents, loc))
      | _ => None
      }
  );

let unescapeString = (x) => {
  let contents = String.sub(x, 1, String.length(x) - 2);
  if (String.length(contents) == 1) {
    contents
  } else {
    Scanf.unescaped(contents)
  }
};

let unescapeChar = (x) =>
  if (String.length(x) == 1) {
    x.[0]
  } else {
    unescapeString(x).[0]
  };

exception ConversionFailure(string);

let unwrap = (opt) =>
  switch opt {
  | Some(x) => x
  | None => raise(ConversionFailure("Unwrapping none"))
  };

let assertEq = (one, two) =>
  if (one != two) {
    raise(ConversionFailure("Assertion error"))
  };
