module exec_OCaml[E]
open exec[E]

sig Exec_OCaml extends Exec {
  A : set E,            // atomic events
}{

  // initial writes are non-atomic
  A in EV - IW
    
  // RMWs and fences are atomic
  (F + (R & W)) in A

  // no concept of atomic or non-atomic locations
  no NAL

}

one sig rm_A extends PTag {}

fun A[e:PTag->E, X:Exec_OCaml] : set E { X.A - e[rm_EV] - e[rm_A] }
