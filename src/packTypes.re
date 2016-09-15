type grammar = list production
and production = (string, list choice) /* rule name -> e1 | e2 | ... */
and choice = (string, string, list parsing) /* choice name, comment, sequence */
and parsing =
  | Star parsing (option string)      /* e* */
  | Plus parsing (option string)      /* e+ */
  | Optional parsing (option string)  /* e? */
  | Lookahead parsing  /* &e */
  | Not parsing        /* !e */
  | NonTerminal string (option string)/* nonterminal 'name' */
  | Terminal string (option string)   /* terminal */
  | Chars char char (option string)   /* [a-z] */
  | Empty              /* epsilon */;
