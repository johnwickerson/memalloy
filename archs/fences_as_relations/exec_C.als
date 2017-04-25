module exec_C[E]
open exec[E]

sig Exec_C extends Exec {
  A : set E,            // atomic events
  acq, rel, sc : set E, // acquire, release, sc events
}{

  // initial writes are non-atomic
  A in ev - IW

  // acquires, releases, and SC operations are all atomic
  acq + rel + sc in A

  // RMWs and fences are atomic
  (F + (R & W)) in A
    
  // sc reads can acquire
  (R & sc) in acq

  // only reads and fences can acquire
  acq in (R + F)
    
  // sc writes can release
  (W & sc) in rel

  // only writes and fences can release
  rel in (W + F)
    
  // sc fences can acquire and release
  (F & sc) in (acq & rel)

  // atomic events do not access non-atomic locations
  no (A & naL)

  // non-atomic reads do not access atomic locations
  R-A in naL

}

pred wf_Exec_C[X:Exec_C, ad,cd,dd:E->E] {
  wf_Exec[X,ad,cd,dd]
}

fun A[e:E, X:Exec_C, ad,cd,dd:E->E] : set E { X.A - e }
fun acq[e:E, X:Exec_C, ad,cd,dd:E->E] : set E { X.acq - e }
fun rel[e:E, X:Exec_C, ad,cd,dd:E->E] : set E { X.rel - e }
fun sc[e:E, X:Exec_C, ad,cd,dd:E->E] : set E { X.sc - e }

pred wf_s[e:E, X:Exec_C, ad,cd,dd,s:E->E] { 

  // s is restricted to sc events
  s in sc[e,X,ad,cd,dd] -> sc[e,X,ad,cd,dd]
    
  // s is acylic
  is_acyclic[s]
    
  // s is transitive
  transitive[s]

  // s is a strict total relation on sc events
  (all e1, e2 : (sc[e,X,ad,cd,dd]) | (e1 != e2) iff (e1 -> e2 in (s + ~s)))
}
