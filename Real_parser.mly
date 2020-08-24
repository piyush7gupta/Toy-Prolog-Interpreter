%{ open Mainfile;; %}

%token LPAREN RPAREN EOL IFF COMMA EOF
%token <string> VAR ID
%start main             
%type <Mainfile.clause> main
%%
main:
    clause EOL { $1 }
    | EOF {(Node(("file_end",0),[]),[])}
;
clause:
    atom { ($1,[]) }
    | atom IFF body { ($1,$3) }
;
atom:
     term   {$1}
    | LPAREN atom RPAREN  {$2}
    
;
body:
     atom   { [$1] }
    | atom COMMA body       { $1::$3 } 
;
term:
     ID { Node(($1,0),[])}
    | VAR { V($1) }
    | ID LPAREN termlist RPAREN { Node(($1,(list_size $3)),$3)}
;
termlist:
     term     { [$1] }
    | term COMMA termlist { $1::$3 }
;
