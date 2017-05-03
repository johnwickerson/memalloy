module exec_OCaml[E]
open exec[E]

sig Exec_OCaml extends Exec {
  ATO : set E,            // atomic events
}{

  // initial writes are non-atomic
  ATO in EV - IW
    
  // RMWs and fences are atomic
  (F + (R & W)) in ATO

  // no concept of atomic or non-atomic locations
  no NAL

}

fun ATO[e:E, X:Exec_OCaml] : set E { X.ATO - e }
