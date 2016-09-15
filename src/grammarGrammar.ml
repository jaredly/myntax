(** This grammar definition was generated from parsable/grammar **)

open PackTypes.Parsing

let grammar = [("Start",
  [("", "",
    [(Star ((NonTerminal ("Rule", None)),
        None))
      ])
    ]);
  ("Decorator",
   [("", "",
     [(Terminal ("@", None));
       (NonTerminal ("ident", Some ("name")));
       (Optional (
          (Group
             [(Terminal ("(", None));
               (Star (
                  (Group
                     [(NonTerminal ("decarg", Some ("args")
                         ));
                       (Terminal (",", None))]),
                  None));
               (Optional (
                  (NonTerminal ("decarg", Some ("args"))),
                  None));
               (Terminal (")", None))]),
          None));
       (NonTerminal ("Comment_eol", None))])
     ]);
  ("decarg",
   [("", "", [(NonTerminal ("bool", None))]);
     ("", "", [(NonTerminal ("string", None))]);
     ("", "", [(NonTerminal ("number", None))])]);
  ("bool",
   [("", "", [(Terminal ("true", None))]);
     ("", "", [(Terminal ("false", None))])]);
  ("Rule",
   [("", "",
     [(Optional (
         (NonTerminal ("Comment_eol", None)), None));
       (Optional (
          (NonTerminal ("Decorator", None)), None));
       (NonTerminal ("ident", Some ("name")));
       (Terminal ("=", None));
       (NonTerminal ("Choice", Some ("choices")));
       (NonTerminal ("Comment_eol", None))]);
     ("", "",
      [(Optional (
          (NonTerminal ("Comment_eol", None)), None));
        (Optional (
           (NonTerminal ("Decorator", None)), None));
        (NonTerminal ("ident", Some ("name")));
        (Terminal ("=", None));
        (NonTerminal ("Comment_eol", None));
        (Plus (
           (Group
              [(Terminal ("|", None));
                (NonTerminal ("Choice", Some ("choices")));
                (NonTerminal ("Comment_eol", None))]),
           None))
        ])
     ]);
  ("Choice",
   [("", "",
     [(Plus (
         (NonTerminal ("Item", Some ("children"))), None));
       (Optional (
          (Group
             [(Terminal ("--", None));
               (NonTerminal ("ident", Some ("name")))]),
          None));
       (Optional (
          (Group
             [(Terminal (";", None));
               (NonTerminal ("rest_of_line",
                  Some ("comment")))
               ]),
          None))
       ])
     ]);
  ("Item",
   [("", "",
     [(Optional (
         (Terminal ("~", Some ("neg"))), None));
       (Optional (
          (Terminal ("#", Some ("lexify"))), None));
       (Optional (
          (Group
             [(Terminal ("[", None));
               (Optional (
                  (NonTerminal ("flag", Some ("flag"))),
                  None));
               (NonTerminal ("ident", Some ("name")));
               (Terminal ("]", None))]),
          None));
       (NonTerminal ("ItemInner", Some ("inner")));
       (Optional (
          (NonTerminal ("suffix", Some ("suffix"))), None))
       ])
     ]);
  ("ItemInner",
   [("", "", [(NonTerminal ("string", None))]);
     ("", "", [(NonTerminal ("ident", None))]);
     ("nested", "",
      [(Terminal ("(", None));
        (Plus (
           (NonTerminal ("Item", Some ("nested"))), None));
        (Terminal (")", None))]);
     ("", "", [(NonTerminal ("char_range", None))]);
     ("", "", [(NonTerminal ("char", None))])]);
  ("char_range",
   [("", "",
     [(Terminal ("'", None));
       (NonTerminal ("single", Some ("start")));
       (Terminal ("..", None));
       (NonTerminal ("single", Some ("end")));
       (Terminal ("'", None))])
     ]);
  ("char",
   [("", "",
     [(Terminal ("'", None));
       (NonTerminal ("single", Some ("char")));
       (Terminal ("'", None))])
     ]);
  ("single",
   [("", "",
     [(Terminal ("\\", None)); (Any None)
       ]);
     ("", "",
      [(Not (Terminal ("'", None)));
        (Not (Terminal ("\n", None)));
        (Any None)])
     ]);
  ("string",
   [("", "",
     [(Terminal ("\"", None));
       (Star (
          (NonTerminal ("strchar", Some ("contents"))),
          None));
       (Terminal ("\"", None))])
     ]);
  ("strchar",
   [("", "",
     [(Terminal ("\\", None)); (Any None)
       ]);
     ("", "",
      [(Not (Terminal ("\"", None)));
        (Not (Terminal ("\n", None)));
        (Any None)])
     ]);
  ("flag",
   [("bool", "exists", [(Terminal ("?", None))]);
     ("array", "", [(Terminal (":", None))]);
     ("string", "contents", [(Terminal ("@", None))])]);
  ("suffix",
   [("plus", "", [(Terminal ("+", None))]);
     ("star", "", [(Terminal ("*", None))]);
     ("opt", "", [(Terminal ("?", None))])]);
  ("ident",
   [("", "",
     [(Not (NonTerminal ("digit", None)));
       (Plus (
          (NonTerminal ("identchar", None)), None))
       ])
     ]);
  ("identchar",
   [("", "", [(Chars ('a', 'z', None))]);
     ("", "", [(Chars ('A', 'Z', None))]);
     ("", "", [(Chars ('0', '9', None))]);
     ("", "", [(Terminal ("_", None))])]);
  ("number",
   [("", "",
     [(Terminal ("0", None));
       (Not
          (NonTerminal ("identchar", None)))
       ]);
     ("", "",
      [(Not (Terminal ("0", None)));
        (Plus (
           (NonTerminal ("digit", None)), None));
        (Not
           (NonTerminal ("identchar", None)))
        ])
     ]);
  ("digit", [("", "", [(Chars ('0', '9', None))])]);
  ("rest_of_line",
   [("", "",
     [(Star (
         (Group
            [(Not (Terminal ("\n", None)));
              (Any None)]),
         None))
       ])
     ]);
  ("Comment_eol",
   [("", "", [(NonTerminal ("One_comment", None))])]);
  ("One_comment",
   [("", "",
     [(Plus (
         (Group
            [(Terminal (";", None));
              (NonTerminal ("rest_of_line", None));
              (NonTerminal ("eee", None))]),
         None))
       ]);
     ("", "", [(NonTerminal ("eee", None))])]);
  ("eol",
   [("", "",
     [(Star (
         (NonTerminal ("white", None)), None));
       (NonTerminal ("eee", None))])
     ]);
  ("eee",
   [("", "",
     [(Plus (
         (NonTerminal ("eolchar", None)), None))
       ]);
     ("", "", [EOF])]);
  ("eolchar",
   [("", "", [(Terminal ("\n", None))]);
     ("", "", [(Terminal ("\r", None))])]);
  ("white",
   [("", "", [(Terminal (" ", None))]);
     ("", "", [(Terminal ("\t", None))])])
  ];

