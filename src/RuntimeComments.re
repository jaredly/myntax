
open Lexing;

let incLine = pos => {...pos, pos_cnum: pos.pos_cnum + 1, pos_lnum: pos.pos_lnum + 1, pos_bol: pos.pos_cnum + 1};

let rec skipWhite = (pos, text, len, ignoreNewlines) =>
  if (pos.pos_cnum >= len) {
    pos
  } else {
    switch (text.[pos.pos_cnum]) {
    | ' ' => skipWhite({...pos, pos_cnum: pos.pos_cnum + 1}, text, len, ignoreNewlines)
    | '\t' => skipWhite({...pos, pos_cnum: pos.pos_cnum + 1}, text, len, ignoreNewlines)
    | '\n' when ignoreNewlines => {
      /* incLine(i + 1); */
      skipWhite(incLine(pos), text, len, ignoreNewlines)
    }
    | _ => pos
    }
  };

let skipALineComment = (pos, start, text, len) => {
  let sl = String.length(start);
  /* TODO maybe iterate? */
  if (sl + pos.pos_cnum < len && String.sub(text, pos.pos_cnum, sl) == start) {
    try ({
      let l = String.index_from(text, pos.pos_cnum, '\n');
      /* incLine(l + 1); */
      {...pos, pos_cnum: l + 1, pos_lnum: pos.pos_lnum + 1, pos_bol: l + 1}
    }) {
    | Not_found => {...pos, pos_cnum: len} /* go to end */
    }
  } else {
    pos
  }
};

let skipABlockComment = (pos, (first, last), text, len) => {
  let fl = String.length(first);
  /* this might be a lot faster with a regex */
  if (fl + pos.pos_cnum < len && String.sub(text, pos.pos_cnum, fl) == first) {
    let fc = last.[0];
    let ll = String.length(last);
    let rec loop = (pos) =>
      if (pos.pos_cnum + ll >= len) {
        failwith("Unterminated comment")
      } else if (text.[pos.pos_cnum] == fc && String.sub(text, pos.pos_cnum, ll) == last) {
        {...pos, pos_cnum: pos.pos_cnum + ll}
      } else {
        if (text.[pos.pos_cnum] == '\n') {
          incLine(pos)
        } else {
          loop({...pos, pos_cnum: pos.pos_cnum + 1})
        }
      };
    loop(pos)
  } else {
    pos
  }
};

/* If we're skipping line comments, we can also skip newlines */
let rec skipLineComments = (pos, start, text, len) => {
  let pos' = skipALineComment(pos, start, text, len);
  if (pos'.pos_cnum == len || pos.pos_cnum == pos'.pos_cnum) {
    pos'
  } else {
    let pos'' = skipWhite(pos', text, len, true);
    if (pos''.pos_cnum > pos'.pos_cnum) {
      skipLineComments(pos'', start, text, len)
    } else {
      pos''
    }
  }
};

let rec skipBlockComments = (pos, ends, text, len, skipNewlines) => {
  let pos' = skipABlockComment(pos, ends, text, len);
  if (pos'.pos_cnum == len || pos.pos_cnum == pos'.pos_cnum) {
    pos'
  } else {
    let pos'' = skipWhite(pos', text, len, skipNewlines);
    if (pos''.pos_cnum > pos'.pos_cnum) {
      skipBlockComments(pos'', ends, text, len, skipNewlines)
    } else {
      pos''
    }
  }
};

let rec skipBlockAndLineComments = (pos, ends, line, text, len) => {
  let pos' = skipABlockComment(pos, ends, text, len);
  let pos' = skipWhite(pos', text, len, true);
  let pos' = skipALineComment(pos', line, text, len);
  if (pos'.pos_cnum == pos.pos_cnum) {
    pos'
  } else {
    skipBlockAndLineComments(pos', ends, line, text, len)
  }
};

let skipAllWhite = (pos, grammar, input, len) => {
  let pos = skipWhite(pos, input, len, true);
  let pos' =
    switch (grammar.PackTypes.Parsing.blockComment, grammar.PackTypes.Parsing.lineComment) {
    | (Some(x), None) => skipBlockComments(pos, x, input, len, true)
    | (Some(x), Some(y)) =>
      skipBlockAndLineComments(pos, x, y, input, len)
    | (None, Some(x)) => skipLineComments(pos, x, input, len)
    | (None, None) => pos
    };
  pos'
};

