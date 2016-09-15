(** This grammar definition was generated from parsable/grammar **)

open PackTypes.Parsing

let grammar = { lineComment = Some (";");
  blockComment = Some (("/*", "*/"));
  rules =
  [("Start",
    { passThrough = false;
      ignoreNewlines = Inherit; leaf = false;
      choices =
      [("", "",
        [(Star (
            (NonTerminal ("Rule", None)), None))
          ])
        ]
      });
    ("Decorator",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
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
                      (NonTerminal ("decarg", Some ("args")
                         )),
                      None));
                   (Terminal (")", None))]),
              None));
           CommentEOL])
         ]
       });
    ("decarg",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("bool", "", [(NonTerminal ("bool", None))]);
         ("string", "", [(NonTerminal ("string", None))]);
         ("number", "", [(NonTerminal ("number", None))])]
       });
    ("bool",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
       choices =
       [("", "", [(Terminal ("true", None))]);
         ("", "", [(Terminal ("false", None))])]
       });
    ("Rule",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Star (
             (NonTerminal ("Decorator", Some ("decorators")
                )),
             None));
           (NonTerminal ("ident", Some ("name")));
           (Terminal ("=", None));
           (NonTerminal ("Choice", Some ("choices")));
           CommentEOL]);
         ("", "",
          [(Star (
              (NonTerminal ("Decorator",
                 Some ("decorators"))),
              None));
            (NonTerminal ("ident", Some ("name")));
            (Terminal ("=", None));
            CommentEOL;
            (Plus (
               (Group
                  [(Terminal ("|", None));
                    (NonTerminal ("Choice",
                       Some ("choices")));
                    CommentEOL]),
               None))
            ])
         ]
       });
    ("Choice",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Plus (
             (NonTerminal ("Item", Some ("children"))),
             None));
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
       ignoreNewlines = Inherit; leaf = false;
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
              (NonTerminal ("suffix", Some ("suffix"))),
              None))
           ])
         ]
       });
    ("ItemInner",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "", [(NonTerminal ("string", None))]);
         ("", "", [(NonTerminal ("ident", None))]);
         ("nested", "",
          [(Terminal ("(", None));
            (NonTerminal ("NestedItems", None));
            (Terminal (")", None))]);
         ("", "", [(NonTerminal ("char_range", None))]);
         ("", "", [(NonTerminal ("char", None))])]
       });
    ("NestedItems",
     { passThrough = true;
       ignoreNewlines = Yes; leaf = false;
       choices =
       [("", "",
         [(Plus (
             (NonTerminal ("Item", Some ("nested"))), None
             ))
           ])
         ]
       });
    ("char_range",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
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
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Terminal ("'", None));
           (NonTerminal ("single", Some ("char")));
           (Terminal ("'", None))])
         ]
       });
    ("single",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
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
       ignoreNewlines = Inherit; leaf = true;
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
       ignoreNewlines = Inherit; leaf = false;
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
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("bool", "exists", [(Terminal ("?", None))]);
         ("array", "", [(Terminal (":", None))]);
         ("string", "contents", [(Terminal ("@", None))])]
       });
    ("suffix",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("plus", "", [(Terminal ("+", None))]);
         ("star", "", [(Terminal ("*", None))]);
         ("opt", "", [(Terminal ("?", None))])]
       });
    ("ident",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
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
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "", [(Chars ('a', 'z', None))]);
         ("", "", [(Chars ('A', 'Z', None))]);
         ("", "", [(Chars ('0', '9', None))]);
         ("", "", [(Terminal ("_", None))])]
       });
    ("number",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
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
       ignoreNewlines = Inherit; leaf = false;
       choices = [("", "", [(Chars ('0', '9', None))])] });
    ("rest_of_line",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
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
    ("eol",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Star (
             (NonTerminal ("white", None)), None));
           (NonTerminal ("eee", None))])
         ]
       });
    ("eee",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Plus (
             (NonTerminal ("eolchar", None)), None))
           ]);
         ("", "", [EOF])]
       });
    ("eolchar",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "", [(Terminal ("\n", None))]);
         ("", "", [(Terminal ("\r", None))])]
       });
    ("white",
     { passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "", [(Terminal (" ", None))]);
         ("", "", [(Terminal ("\t", None))])]
       })
    ]
  };
