(*
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
*)

open Format

let set_list_ref r v = r := (v :: !r)
							 
let get_only_element k = function [x] -> x | _ -> k ()

let rec fprintf_iter s f oc = function
  | [] -> ()
  | [x] -> fprintf oc "(%a)" f x
  | x :: xs -> fprintf oc "(%a) %s %a" f x s (fprintf_iter s f) xs

let debug b format =
  if b then eprintf format else ifprintf err_formatter format

let today() =
  let open Unix in
  let t = localtime (time ()) in
  sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday

let now() =
  let open Unix in
  let t = localtime (time ()) in
  sprintf "%02d:%02d:%02d" t.tm_hour t.tm_min t.tm_sec

let chop_extension extn path =
  if Filename.check_suffix path extn then
    Filename.chop_extension path
  else
    failwith
      (asprintf "File %s does not have extension \"%s\"." path extn)
