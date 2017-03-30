module exec_OCaml[E]
open exec[E]

sig Exec_OCaml extends Exec {
  A : set E,            // atomic events
}{

  // initial writes are non-atomic
  A in ev - IW
    
  // RMWs and fences are atomic
  (F + (R & W)) in A

  // no concept of atomic or non-atomic locations
  no naL

}

pred wf_Exec_OCaml[X:Exec_OCaml, ad,cd,dd:E->E] {
  wf_Exec[X,ad,cd,dd]
}

fun A[e:E, X:Exec_OCaml, ad,cd,dd:E->E] : set E { X.A - e }
