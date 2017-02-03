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

open Format
open General_purpose

(*****************************)
(* Datatype for litmus tests *)
(*****************************)

type attribute = string
type register = int
type location = int
type address = Reg of register | Loc of location
type value = int
       
type 'a with_fake_deps = 'a * register list
		   
type instruction =
  | Load of register * location with_fake_deps * attribute list
  | Store of location_expr * value with_fake_deps * attribute list
  | Cas of location_expr * value * value with_fake_deps * attribute list
  | Fence of attribute list
		   
type component =
  | Instr of instruction
  | Seq of component list
  | Unseq of component list
  | If of reg * value * instruction
		       
type litmus_test = {
    locs: location list;
    thds: component list;
    post: (address * value) list;
  }
