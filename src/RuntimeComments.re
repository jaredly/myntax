
open Lexing;
module R = PackTypes.Result;


let mkLoc = (a, b) => {
  Location.loc_start: a,
  loc_end: b,
  loc_ghost: false,
};


let incLine = pos => {...pos, pos_cnum: pos.pos_cnum + 1, pos_lnum: pos.pos_lnum + 1, pos_bol: pos.pos_cnum + 1};

let rec skipWhite = (pos, text, len, ignoreNewlines) =>
  if (pos.pos_cnum >= len) {
    pos;
  } else {
    switch (text.[pos.pos_cnum]) {
    | ' ' => skipWhite({...pos, pos_cnum: pos.pos_cnum + 1}, text, len, ignoreNewlines)
    | '\t' => skipWhite({...pos, pos_cnum: pos.pos_cnum + 1}, text, len, ignoreNewlines)
    | '\n' when ignoreNewlines =>
      /* incLine(i + 1); */
      skipWhite(incLine(pos), text, len, ignoreNewlines)
    | _ => pos
    };
  };

let skipALineComment = (pos, start, text, len) => {
  let sl = String.length(start);
  /* TODO maybe iterate? */
  if (sl + pos.pos_cnum < len && String.sub(text, pos.pos_cnum, sl) == start) {
    try (
      {
        let l = String.index_from(text, pos.pos_cnum, '\n');
        let final = {...pos, pos_cnum: l + 1, pos_lnum: pos.pos_lnum + 1, pos_bol: l + 1};
        /* incLine(l + 1); */
        (final, Some(R.Comment(R.EOL, String.sub(text, pos.pos_cnum, l - pos.pos_cnum), mkLoc(pos, final))));
      }
    ) {
    | Not_found =>
      let final = {...pos, pos_cnum: len};
      (final, Some(R.Comment(R.EOL, String.sub(text, pos.pos_cnum, len - pos.pos_cnum), mkLoc(pos, final)))); /* go to end */
    };
  } else {
    (pos, None);
  };
};

let skipABlockComment = (pos, (first, last), text, len) => {
  let fl = String.length(first);
  /* this might be a lot faster with a regex */
  if (fl + pos.pos_cnum < len && String.sub(text, pos.pos_cnum, fl) == first) {
    let p0 = pos;
    let fc = last.[0];
    let ll = String.length(last);
    let rec loop = pos =>
      if (pos.pos_cnum + ll >= len) {
        failwith("Unterminated comment");
      } else if (text.[pos.pos_cnum] == fc && String.sub(text, pos.pos_cnum, ll) == last) {
        let final = {...pos, pos_cnum: pos.pos_cnum + ll};
        (final, Some(R.Comment(R.Multi, String.sub(text, p0.pos_cnum, final.pos_cnum - p0.pos_cnum), mkLoc(p0, final))))
      } else if (text.[pos.pos_cnum] == '\n') {
        loop(incLine(pos))
      } else {
        loop({...pos, pos_cnum: pos.pos_cnum + 1});
      };
    loop(pos);
  } else {
    (pos, None)
  };
};

/* If we're skipping line comments, we can also skip newlines */
let rec skipLineComments = (pos, start, text, len) => {
  let (pos', contents) = skipALineComment(pos, start, text, len);
  switch contents {
    | None => (pos', [])
    | Some(item) =>
      if (pos'.pos_cnum == len || pos.pos_cnum == pos'.pos_cnum) {
        (pos', [item])
      } else {
        let pos'' = skipWhite(pos', text, len, true);
        if (pos''.pos_cnum > pos'.pos_cnum) {
          let (p, items) = skipLineComments(pos'', start, text, len);
          (p, [item, ...items])
        } else {
          (pos'', [item])
        }
      }
  }
};

let rec skipBlockComments = (pos, ends, text, len, skipNewlines) => {
  let (pos', contents) = skipABlockComment(pos, ends, text, len);
  switch contents {
    | None => (pos', [])
    | Some(item) =>
    if (pos'.pos_cnum == len || pos.pos_cnum == pos'.pos_cnum) {
      (pos', [item])
    } else {
      let pos'' = skipWhite(pos', text, len, skipNewlines);
      if (pos''.pos_cnum > pos'.pos_cnum) {
        let (p, items) = skipBlockComments(pos'', ends, text, len, skipNewlines);
        (p, [item, ...items])
      } else {
        (pos'', [item])
      }
    }
  }
};

let rec skipBlockAndLineComments = (pos, ends, line, text, len) => {
  let (pos', block) = skipABlockComment(pos, ends, text, len);
  let pos' = skipWhite(pos', text, len, true);
  let (pos', eol) = skipALineComment(pos', line, text, len);
  if (pos'.pos_cnum == pos.pos_cnum) {
    (pos', [])
  } else {
    let (p, items) = skipBlockAndLineComments(pos', ends, line, text, len);
    switch (block, eol) {
      | (None, None) => (p, items)
      | (Some(b), Some(l)) => (p, [b, l, ...items])
      | (None, Some(l)) => (p, [l, ...items])
      | (Some(b), None) => (p, [b, ...items])
    }
  }
};

let skipAllWhite = (pos, grammar, input, len) => {
  let pos = skipWhite(pos, input, len, true);
    switch (grammar.PackTypes.Parsing.blockComment, grammar.PackTypes.Parsing.lineComment) {
    | (Some(x), None) => skipBlockComments(pos, x, input, len, true)
    | (Some(x), Some(y)) =>
      skipBlockAndLineComments(pos, x, y, input, len)
    | (None, Some(x)) => skipLineComments(pos, x, input, len)
    | (None, None) => (pos, [])
    };
};

