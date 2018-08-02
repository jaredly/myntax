(** This grammar definition was generated from parsable/grammar **)
open PackTypes.Parsing

let grammar = { lineComment = Some (";");
  blockComment = Some (("/*", "*/"));
  rules =
  [("Start",
    { docs = None; passThrough = false;
      ignoreNewlines = Inherit; leaf = false;
      choices =
      [("", "",
        [(Star
            (NonTerminal ("Rule", None)))
          ])
        ]
      });
    ("Decorator",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Terminal ("@", None));
           (NonTerminal ("ident", Some ("name")));
           (Optional
              (Group
                 [(Terminal ("(", None));
                   (Star
                      (Group
                         [(NonTerminal ("decarg",
                             Some ("args")));
                           (Terminal (",", None))]));
                   (Optional
                      (NonTerminal ("decarg", Some ("args")
                         )));
                   (Terminal (")", None))]));
           CommentEOL])
         ]
       });
    ("decarg",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("bool", "", [(NonTerminal ("bool", None))]);
         ("string", "", [(NonTerminal ("string", None))]);
         ("number", "", [(NonTerminal ("number", None))])]
       });
    ("bool",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
       choices =
       [("", "", [(Terminal ("true", None))]);
         ("", "", [(Terminal ("false", None))])]
       });
    ("Rule",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Star
             (NonTerminal ("Decorator", Some ("decorators")
                )));
           (NonTerminal ("ident", Some ("name")));
           (Terminal ("=", None));
           (NonTerminal ("Choice", Some ("choices")));
           CommentEOL]);
         ("", "",
          [(Star
              (NonTerminal ("Decorator",
                 Some ("decorators"))));
            (NonTerminal ("ident", Some ("name")));
            (Terminal ("=", None));
            CommentEOL;
            (Plus
               (Group
                  [(Terminal ("|", None));
                    (NonTerminal ("Choice",
                       Some ("choices")));
                    CommentEOL]))
            ])
         ]
       });
    ("Choice",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Plus
             (NonTerminal ("Item", None)));
           (Optional
              (Group
                 [(Terminal ("--", None));
                   (NonTerminal ("ident", Some ("name")))]));
           (Optional
              (Group
                 [(Terminal (";", None));
                   (NonTerminal ("rest_of_line",
                      Some ("comment")))
                   ]))
           ])
         ]
       });
    ("Item",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Optional
             (NoSpaceAfter
                (Terminal ("~", Some ("neg")))));
           (Optional
              (NoSpaceAfter
                 (Terminal ("#", Some ("lexify")))));
           (Optional
              (NoSpaceAfter
                 (Group
                    [(NoSpaceAfter
                        (Terminal ("[", None)));
                      (Optional
                         (NoSpaceAfter
                            (NonTerminal ("flag",
                               Some ("flag")))));
                      (NoSpaceAfter
                         (NonTerminal ("ident",
                            Some ("name"))));
                      (Terminal ("]", None))])));
           (Optional
              (NoSpaceAfter
                 (NonTerminal ("noSpace",
                    Some ("noSpaceBefore")))));
           (NonTerminal ("ItemInner", Some ("inner")));
           (Optional
              (NoSpaceBefore
                 (NonTerminal ("noSpace",
                    Some ("noSpaceAfter")))));
           (Optional
              (NoSpaceBefore
                 (NonTerminal ("suffix", Some ("suffix")))))
           ])
         ]
       });
    ("noSpace",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
       choices = [("", "", [(Terminal ("&", None))])] });
    ("ItemInner",
     { docs = None; passThrough = false;
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
     { docs = None; passThrough = true;
       ignoreNewlines = Yes; leaf = false;
       choices =
       [("", "",
         [(Plus
             (NonTerminal ("Item", Some ("nested"))))
           ])
         ]
       });
    ("char_range",
     { docs = None; passThrough = false;
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
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Terminal ("'", None));
           (NonTerminal ("single", Some ("char")));
           (Terminal ("'", None))])
         ]
       });
    ("single",
     { docs = None; passThrough = false;
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
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
       choices =
       [("", "",
         [(Terminal ("\"", None));
           (Star
              (NonTerminal ("strchar", Some ("contents"))));
           (Terminal ("\"", None))])
         ]
       });
    ("strchar",
     { docs = None; passThrough = false;
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
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("bool", "exists", [(Terminal ("?", None))]);
         ("array", "", [(Terminal (":", None))]);
         ("string", "contents", [(Terminal ("@", None))])]
       });
    ("suffix",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("plus", "", [(Terminal ("+", None))]);
         ("star", "", [(Terminal ("*", None))]);
         ("opt", "", [(Terminal ("?", None))])]
       });
    ("ident",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
       choices =
       [("", "",
         [(Not
             (NonTerminal ("digit", None)));
           (Plus
              (NonTerminal ("identchar", None)))
           ])
         ]
       });
    ("identchar",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "", [(Chars ('a', 'z', None))]);
         ("", "", [(Chars ('A', 'Z', None))]);
         ("", "", [(Chars ('0', '9', None))]);
         ("", "", [(Terminal ("_", None))])]
       });
    ("number",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
       choices =
       [("", "",
         [(Terminal ("0", None));
           (Not
              (NonTerminal ("identchar", None)))
           ]);
         ("", "",
          [(Not (Terminal ("0", None)));
            (Plus
               (NonTerminal ("digit", None)));
            (Not
               (NonTerminal ("identchar", None)))
            ])
         ]
       });
    ("digit",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices = [("", "", [(Chars ('0', '9', None))])] });
    ("rest_of_line",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = true;
       choices =
       [("", "",
         [(Star
             (Group
                [(Not
                    (Terminal ("\n", None)));
                  (Any None)]))
           ])
         ]
       });
    ("eol",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Star
             (NonTerminal ("white", None)));
           (NonTerminal ("eee", None))])
         ]
       });
    ("eee",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "",
         [(Plus
             (NonTerminal ("eolchar", None)))
           ]);
         ("", "", [EOF])]
       });
    ("eolchar",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "", [(Terminal ("\n", None))]);
         ("", "", [(Terminal ("\r", None))])]
       });
    ("white",
     { docs = None; passThrough = false;
       ignoreNewlines = Inherit; leaf = false;
       choices =
       [("", "", [(Terminal (" ", None))]);
         ("", "", [(Terminal ("\t", None))])]
       })
    ]
  };

