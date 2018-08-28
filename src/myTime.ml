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

open! Format
open! General_purpose

(** For timing and generating timestamps *)
       
let today () =
  let t = Unix.localtime (Unix.time ()) in
  sprintf "%04d-%02d-%02d"
    (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday

let now () =
  let t = Unix.localtime (Unix.time ()) in
  sprintf "%02d:%02d:%02d"
    t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

(** Return the number of milliseconds since the Unix epoch *)
let now_ms () =
  int_of_float (Unix.gettimeofday() *. 1000.0)

let timestamp () =
  let t = Unix.localtime (Unix.time ()) in
  sprintf "%02d%02d%02d-%02d%02d%02d"
    (t.Unix.tm_year mod 100) (t.Unix.tm_mon + 1) t.Unix.tm_mday
    t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let timing_report = ref []

let timer_started = ref None                       

let start_timer name =
  match !timer_started with
  | None -> timer_started := Some (name, now_ms())
  | Some (name,_) -> failwith "ERROR: Timer '%s' already started!" name

let stop_timer () =
  match !timer_started with
  | None -> failwith "ERROR: Timer not started!"
  | Some (name, start_time) ->
     timing_report := !timing_report @ [name, now_ms() - start_time];
     timer_started := None

let report_timings () =
  List.iter (fun (l,t) -> printf "%13s time: %d ms\n" l t) !timing_report;
  
