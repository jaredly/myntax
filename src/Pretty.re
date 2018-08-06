/** Taken with modifications from https://github.com/t0yv0/ocaml-pretty/blob/master/pretty.ml */;

type breakMode = Normal | CannotFlatten | BreakAfter;

type doc = {
  /*** The document node */
  node,
  /*** Governs how flattening can occur */
  break_mode: breakMode,
  /*** Document size when flattened */
  flat_size: int,
  /*** Minimal width of the first line */
  min_width: int,
  /*** True if document contains no newline nodes */
  single_line: bool,
}
and node =
  | Append(doc, doc)
  | Empty
  | Group(doc)
  | Indent(int, doc) /* indent this whole doc */
  | BackLine(int, string) /* the string is what to print if we *don't* break */
  | Line(int, string) /* the string is what to print if we *dont't* break */
  | Text(int, string); /* int = String.length string */

let append = (left, right) =>
  switch (left.node, right.node) {
  | (Empty, _) => right
  | (_, Empty) => left
  | _ => {
      node: Append(left, right),
      break_mode: switch (left.break_mode, right.break_mode) {
        | (CannotFlatten, _) | (_, CannotFlatten) => CannotFlatten
        | (BreakAfter, _) => CannotFlatten
        | (_, BreakAfter) => BreakAfter
        | _ => Normal
      },
      flat_size: left.flat_size + right.flat_size,
      min_width:
        if (left.single_line) {
          left.min_width + right.min_width;
        } else {
          left.min_width;
        },
      single_line: left.single_line && right.single_line,
    }
  };

let empty = {node: Empty, flat_size: 0, min_width: 0, single_line: true, break_mode: Normal};

let back = (num, text) => {
  node: BackLine(num, text),
  flat_size: 0,
  min_width: 0,
  single_line: true,
  break_mode: Normal,
};

let group = doc => {...doc, node: Group(doc)};

let indent = (amount, doc) => {
  ...doc,
  node: Indent(amount, doc),
};

let line = defaultString => {
  let length = String.length(defaultString);
  {
    node: Line(length, defaultString),
    flat_size: length,
    min_width: 0,
    single_line: false,
    break_mode: Normal,
  };
};

let text = (~len=?, string) => {
  let len =
    switch (len) {
    | None => String.length(string)
    | Some(n) => n
    };
  {
    node: Text(len, string),
    flat_size: len,
    min_width: len,
    single_line: true,
    break_mode: Normal,
  };
};

let multiLine = (string) => {
  {
    break_mode: CannotFlatten,
    node: Text(0, string),
    single_line: false,
    /* TODO these are lies */
    flat_size: String.length(string),
    min_width: String.length(string)
  }
};

let breakAfter = (~len=?, string) => {
  {...text(~len?, string), break_mode: BreakAfter}
};

let dontFlatten = doc => {...doc, break_mode: CannotFlatten};

let rec flatten = doc =>
  switch (doc.node) {
  | Append(a, b) => append(flatten(a), flatten(b))
  | Empty
  | Text(_) => doc
  | Group(x)
  | Indent(_, x) => flatten(x)
  | Line(_, x) => text(x)
  | BackLine(_, x) => text(x)
  };

type stack_node = {
  doc,
  min_total: int,
  offset: int,
};

type stack =
  | Nil
  | Cons(stack_node, stack);

let min_total = stack =>
  switch (stack) {
  | Nil => 0
  | Cons(head, _) => head.min_total
  };

let push = (offset, node, stack: stack) => {
  let current_min_total =
    if (node.single_line) {
      min_total(stack) + node.min_width;
    } else {
      node.min_width;
    };
  Cons({doc: node, offset, min_total: current_min_total}, stack);
};

let print_indentation = n => {
  /* print_char('\n'); */
  for (i in 1 to n) {
    print_char(' ');
  };
};

let print = (~width=70, ~output=print_string, ~indent=print_indentation, doc) => {
  let rec loop = (currentIndent, stack) =>
    switch (stack) {
    | Nil => ()
    | Cons(stackNode, rest) =>
      let offset = stackNode.offset;
      switch (stackNode.doc.node) {
      | Append(left, right) =>
        loop(currentIndent, push(offset, left, push(offset, right, rest)))
      | Empty => loop(currentIndent, rest)
      | Group(doc) =>
        let flatDoc =
          if (doc.break_mode != CannotFlatten && doc.flat_size + min_total(rest) <= width - currentIndent) {
            flatten(doc);
          } else {
        /* output("---|"); */
            doc;
          };
        /* output("g"); */
        loop(currentIndent, push(offset, flatDoc, rest));
      | Indent(ident, doc) =>
        indent(ident);
        /* output("i"); */
        loop(currentIndent, push(offset + ident, doc, rest))
      | BackLine(num, _) =>
        output("\n");
        indent(offset - num);
        loop(offset - num, rest);
      | Line(_) =>
        output("\n");
        indent(offset);
        loop(offset, rest);
      | Text(len, string) =>
        output(string);
        loop(currentIndent + len, rest);
      };
    };
  loop(0, push(0, doc, Nil));
};
