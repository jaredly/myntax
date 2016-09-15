(** This grammar definition was generated from parsable/grammar **)

open PackTypes.Parsing

let grammar = [("Start",
  { passThrough = false;
    ignoreWhitespace = Inherit;
    choices =
    [("", "",
      [(Star (
          (NonTerminal ("Rule", None)), None))
        ])
      ]
    });
  ("Decorator",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Terminal ("@", None));
         (NonTerminal ("ident", Some ("name")));
         (Optional (
            (Group
               [(Terminal ("(", None));
                 (Star (
                    (Group
                       [(NonTerminal ("decarg",
                           Some ("args")));
                         (Terminal (",", None))]),
                    None));
                 (Optional (
                    (NonTerminal ("decarg", Some ("args"))),
                    None));
                 (Terminal (")", None))]),
            None));
         (NonTerminal ("Comment_eol", None))])
       ]
     });
  ("decarg",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "", [(NonTerminal ("bool", None))]);
       ("", "", [(NonTerminal ("string", None))]);
       ("", "", [(NonTerminal ("number", None))])]
     });
  ("bool",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "", [(Terminal ("true", None))]);
       ("", "", [(Terminal ("false", None))])]
     });
  ("Rule",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
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
                  (NonTerminal ("Choice", Some ("choices")
                     ));
                  (NonTerminal ("Comment_eol", None))]),
             None))
          ])
       ]
     });
  ("Choice",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Plus (
           (NonTerminal ("Item", Some ("children"))), None
           ));
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
       ]
     });
  ("Item",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
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
            (NonTerminal ("suffix", Some ("suffix"))), None
            ))
         ])
       ]
     });
  ("ItemInner",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "", [(NonTerminal ("string", None))]);
       ("", "", [(NonTerminal ("ident", None))]);
       ("nested", "",
        [(Terminal ("(", None));
          (Plus (
             (NonTerminal ("Item", Some ("nested"))), None
             ));
          (Terminal (")", None))]);
       ("", "", [(NonTerminal ("char_range", None))]);
       ("", "", [(NonTerminal ("char", None))])]
     });
  ("char_range",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Terminal ("'", None));
         (NonTerminal ("single", Some ("start")));
         (Terminal ("..", None));
         (NonTerminal ("single", Some ("end")));
         (Terminal ("'", None))])
       ]
     });
  ("char",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Terminal ("'", None));
         (NonTerminal ("single", Some ("char")));
         (Terminal ("'", None))])
       ]
     });
  ("single",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Terminal ("\\", None));
         (Any None)]);
       ("", "",
        [(Not (Terminal ("'", None)));
          (Not (Terminal ("\n", None)));
          (Any None)])
       ]
     });
  ("string",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Terminal ("\"", None));
         (Star (
            (NonTerminal ("strchar", Some ("contents"))),
            None));
         (Terminal ("\"", None))])
       ]
     });
  ("strchar",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Terminal ("\\", None));
         (Any None)]);
       ("", "",
        [(Not (Terminal ("\"", None)));
          (Not (Terminal ("\n", None)));
          (Any None)])
       ]
     });
  ("flag",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("bool", "exists", [(Terminal ("?", None))]);
       ("array", "", [(Terminal (":", None))]);
       ("string", "contents", [(Terminal ("@", None))])]
     });
  ("suffix",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("plus", "", [(Terminal ("+", None))]);
       ("star", "", [(Terminal ("*", None))]);
       ("opt", "", [(Terminal ("?", None))])]
     });
  ("ident",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Not
           (NonTerminal ("digit", None)));
         (Plus (
            (NonTerminal ("identchar", None)), None))
         ])
       ]
     });
  ("identchar",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "", [(Chars ('a', 'z', None))]);
       ("", "", [(Chars ('A', 'Z', None))]);
       ("", "", [(Chars ('0', '9', None))]);
       ("", "", [(Terminal ("_", None))])]
     });
  ("number",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
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
       ]
     });
  ("digit",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices = [("", "", [(Chars ('0', '9', None))])] });
  ("rest_of_line",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Star (
           (Group
              [(Not
                  (Terminal ("\n", None)));
                (Any None)]),
           None))
         ])
       ]
     });
  ("Comment_eol",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "", [(NonTerminal ("One_comment", None))])] });
  ("One_comment",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Plus (
           (Group
              [(Terminal (";", None));
                (NonTerminal ("rest_of_line", None));
                (NonTerminal ("eee", None))]),
           None))
         ]);
       ("", "", [(NonTerminal ("eee", None))])]
     });
  ("eol",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Star (
           (NonTerminal ("white", None)), None));
         (NonTerminal ("eee", None))])
       ]
     });
  ("eee",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "",
       [(Plus (
           (NonTerminal ("eolchar", None)), None))
         ]);
       ("", "", [EOF])]
     });
  ("eolchar",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "", [(Terminal ("\n", None))]);
       ("", "", [(Terminal ("\r", None))])]
     });
  ("white",
   { passThrough = false;
     ignoreWhitespace = Inherit;
     choices =
     [("", "", [(Terminal (" ", None))]);
       ("", "", [(Terminal ("\t", None))])]
     })
  ];

