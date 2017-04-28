(*
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
*)

(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


{
  open Cat_parser
	 
  let remove_hyphens x =
    Str.global_replace (Str.regexp_string "-") "" x

  let remove_dots x =
    Str.global_replace (Str.regexp_string ".") "" x
       
  let check_keyword = function
    | "AA" -> AA | "acyclic" -> ACYCLIC
    | "and" -> AND | "AP" -> AP | "as" -> AS
    | "deadness_requires" -> DEADNESS_REQUIRES
    | "domain" -> DOMAIN
    | "empty" -> TESTEMPTY
    | "include" -> INCLUDE
    | "irreflexive" -> IRREFLEXIVE
    | "let" -> LET
    | "MM" -> MM | "MR" -> MR | "MW" -> MW
    | "rec" -> REC
    | "RM" -> RM | "RR" -> RR | "RW" -> RW 
    | "PA" -> PA | "PP" -> PP
    | "range" -> RANGE
    | "show" -> SHOW
    | "undefined_unless" -> UNDEFINED_UNLESS
    | "unshow" -> UNSHOW
    | "withsc" -> WITHSC
    | "WM" -> WM | "WR" -> WR | "WW" -> WW
    | x -> VAR (remove_hyphens (remove_dots x))
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_'|'.'|'-')* '\''?

rule token = parse
| [' ''\t'] { token lexbuf }
| ['\n']    { token lexbuf }
| "(*"      { skip_comment 0 lexbuf ; token lexbuf }
| '#' [^'\n']* '\n' { token lexbuf }
| '('   { LPAR }
| ')'   { RPAR }
| '['   { LBRAC }
| ']'   { RBRAC }
| '_'   { UNDERSCORE }
| '0'   { EMPTY }
| '|'   { UNION }
| '&'   { INTER }
| '*'   { STAR }
| '~'   { COMP }
| '!'   { NOT }
| '+'   { PLUS }
| '^'   { HAT }
| "-1"  { INV }
| '\\'  { DIFF }
| '?'   { OPT }
| '='   { EQUAL }
| ';'   { SEMI }
| ','   { COMMA }
| '"' ([^'"']* as s) '"' { STRING s }
| name as x { check_keyword x }
| eof { EOF }
| ""  { failwith "Lexing error." }

and skip_comment n = parse
| "(*" { skip_comment (n+1) lexbuf }
| "*)" { if n>0 then skip_comment (n-1) lexbuf }
| eof  { failwith "unclosed comment" }
| _    { skip_comment n lexbuf }

{

}
