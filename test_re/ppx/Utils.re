exception Badness(string);

let fail = (x) => raise(Badness(x));

let optor = (opt, orr) =>
  switch opt {
  | None => orr
  | Some(x) => x
  };
