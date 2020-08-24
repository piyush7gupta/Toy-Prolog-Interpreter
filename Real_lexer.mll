{open Real_parser}
rule token =parse
  [' ' '\t' '\n']                                                    { token lexbuf }    
  | '.'                                                              { EOL       }
  | ','                                                              { COMMA     }
  | '('                                                              { LPAREN    }
  | ')'                                                              { RPAREN    }
  | ":-"                                                             { IFF       }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*    as var            { VAR (var) }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*    as id             { ID (id)   }
  | eof                                                              { EOF       }