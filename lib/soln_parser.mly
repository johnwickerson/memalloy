/*
MIT License

Copyright (c) 2017 by John Wickerson.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

%{
%}
   
%token EOF
%token <string> VAR
%token <int> EVENT
%token LPAR RPAR LBRAC RBRAC LBRACE RBRACE
%token PLUS SEMI COMMA EQUAL 
%token BR SPO X Y PI SINGLE DOUBLE SET REL
                                                
%type <Soln.t> soln
			                               
%start soln
                                                       
%%
  
soln:
| DOUBLE LBRACE X EQUAL exec Y EQUAL exec PI EQUAL rel RBRACE EOF
  { Soln.Double($5,$8,$11) }
| SINGLE LBRACE X EQUAL exec RBRACE EOF
  { Soln.Single($5) }

exec:
| LBRACE SPO EQUAL spo sets rels RBRACE
  { { Exec.sb = $4; Exec.sets = $5; Exec.rels = $6 } }

spo:
| BR LBRAC threads RBRAC { Spo.Br $3 }
| EVENT                  { Spo.Lf () }

threads:
| LBRAC thread RBRAC              { [$2] }
| LBRAC thread RBRAC PLUS threads { $2 :: $5 }

thread:
| spo             { [$1] }
| spo SEMI thread { $1 :: $3 }

sets:
|                        { [] }
| SET VAR EQUAL set sets { ($2, $4) :: $5 }

rels:
|                        { [] }
| REL VAR EQUAL rel rels { ($2, $4) :: $5 }

set:
| LBRACE RBRACE        { [] }
| LBRACE events RBRACE { $2 }

rel:
| LBRACE RBRACE       { [] }
| LBRACE pairs RBRACE { $2 }

events:
| EVENT              { [$1] }
| EVENT COMMA events { $1 :: $3 }

pairs:
| pair             { [$1] }
| pair COMMA pairs { $1 :: $3 }

pair:
| LPAR EVENT COMMA EVENT RPAR { ($2, $4) }
