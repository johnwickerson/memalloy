/*
MIT License

Copyright (c) 2017 by John Wickerson. Some code is inherited from
Herd, for which the copyright is held by Luc Maranget, Jade Alglave,
and John Wickerson. The licence for Herd is given below.

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

/*********************************************************************/
/*                        Herd                                       */
/*                                                                   */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                   */
/* Jade Alglave, University College London, UK.                      */
/* John Wickerson, Imperial College London, UK.                      */
/*                                                                   */
/*  Copyright 2013 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/


%{
open Cat_syntax

let as_op op = function
  | Op (op',es) when op' = op -> es
  | e -> [e]

let do_op op e1 e2 = Op (op, as_op op e1 @ as_op op e2)

%}
   
%token EOF
%token <string> VAR
%token <string> STRING
%token LPAR RPAR LBRAC RBRAC
%token EMPTY UNDERSCORE
%token ALT SEMI UNION INTER COMMA DIFF
%token STAR PLUS OPT INV COMP HAT DOMAIN RANGE
%token AND ACYCLIC AS DEADNESS_REQUIRES EQUAL IRREFLEXIVE
INCLUDE LET REC SHOW TESTEMPTY WITHSC UNDEFINED_UNLESS UNSHOW
       
%type <string * bool * Cat_syntax.cat_model> main
%type <Cat_syntax.shape * Cat_syntax.cat_expr * string> axiom
			       
%start main
%start axiom

/* Precedences */
%right UNION
%right SEMI
%left DIFF
%right INTER
%nonassoc STAR PLUS OPT INV COMP NOT HAT DOMAIN
	  
%%

main:
| model_name opt_withsc ins_list EOF { ($1, $2, $3) }

model_name:
| VAR { $1 }
| STRING   { $1 }

opt_withsc:
|        { false }
| WITHSC { true }

ins_list:
|              { [] }
| ins ins_list { $1 @ $2 }

ins:
| LET VAR EQUAL exp  { [Let ($2,[],$4)] }
| LET REC VAR EQUAL exp more_bindings
                     { [LetRec (($3,$5) :: $6)] }
| LET VAR LPAR var_list RPAR EQUAL exp
                     { [Let ($2,$4,$7)] }
| cnstrnt_type axiom
                     { let s,e,n = $2 in [Axiom($1,s,e,n)] }
| cnstrnt_type test_type exp
                     { failwith "All tests must be named." }
| INCLUDE STRING     { [Include $2] }
| SHOW exp AS VAR    { [] }
| SHOW var_list      { [] }
| UNSHOW var_list    { [] }

axiom:
| test_type exp AS VAR { ($1, $2, $4) }
	 
more_bindings:
|                                 { [] }
| AND VAR EQUAL exp more_bindings { ($2,$4) :: $5 }

cnstrnt_type:
|                   { Provision }
| UNDEFINED_UNLESS  { UndefUnless }
| DEADNESS_REQUIRES { Deadness }

test_type:
| ACYCLIC     { Acyclic }
| IRREFLEXIVE { Irreflexive }
| TESTEMPTY   { IsEmpty }

var_list:
| VAR                { [$1] }
| VAR COMMA var_list { $1 :: $3 }

exp:
| EMPTY                  { Empty }
| UNDERSCORE             { Var "ev" }
| LPAR exp RPAR          { $2 }
| VAR                    { Var ($1) }
| VAR LPAR exp_list RPAR { App ($1,$3) }
| exp STAR exp           { Op (Cross, [$1; $3])}
| LBRAC exp RBRAC        { Op1 (Set_to_rln,$2)}
| exp STAR               { Op1 (Star,$1) }
| exp PLUS               { Op1 (Plus,$1) }
| exp OPT                { Op1 (Opt,$1) }
| exp HAT INV            { Op1 (Inv,$1) }
| exp SEMI exp           { do_op Seq $1 $3 }
| exp UNION exp          { do_op Union $1 $3 }
| exp DIFF exp           { Op (Diff, [$1; $3;]) }
| exp INTER exp          { Op (Inter, [$1; $3;]) }
| COMP exp               { Op1 (Comp, $2) }
| DOMAIN LPAR exp RPAR   { Op1 (Domain,$3) }
| RANGE LPAR exp RPAR    { Op1 (Range,$3) }

exp_list:
| exp                { [$1] }
| exp COMMA exp_list { $1 :: $3 }
	