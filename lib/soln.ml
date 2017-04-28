(*
MIT License

Copyright (c) 2017 by John Wickerson and Tyler Sorensen.

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

(** Representation of an Alloy solution *)

open Format
open General_purpose

(** Either a single execution, or a pair of executions linked by some mapping relation *)
type t =
  | Single of Exec.t
  | Double of Exec.t * Exec.t * Event.t Rel.t

let pp oc = function
  | Single x ->
     fprintf oc
       "Single {\n\
          X = %a;\n\
        }\n"
       Exec.pp x
  | Double (x,y,pi) ->
     fprintf oc
       "Double {\n\
          X = %a;\n\
          Y = %a;\n\
          pi = %a\n\
        }\n"
       Exec.pp x Exec.pp y (Rel.pp Event.pp) pi
