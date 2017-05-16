module exec_C[E]
open exec[E]

sig Exec_C extends Exec {
  A : set E,            // atomic events
  ACQ, REL, SC : set E, // acquire, release, sc events
}{

  // initial writes are non-atomic
  A in EV - IW

  // acquires, releases, and SC operations are all atomic
  ACQ + REL + SC in A

  // RMWs and fences are atomic
  (F + (R & W)) in A
    
  // sc reads can acquire
  (R & SC) in ACQ

  // only reads and fences can acquire
  ACQ in (R + F)
    
  // sc writes can release
  (W & SC) in REL

  // only writes and fences can release
  REL in (W + F)
    
  // sc fences can acquire and release
  (F & SC) in (ACQ & REL)

  // atomic events do not access non-atomic locations
  no (A & NAL)

  // non-atomic reads do not access atomic locations
  R-A in NAL

}

fun A[e:PTag->E, X:Exec_C] : set E { X.A - e[rm_EV] }
fun ACQ[e:PTag->E, X:Exec_C] : set E { X.ACQ - e[rm_EV] }
fun REL[e:PTag->E, X:Exec_C] : set E { X.REL - e[rm_EV] }
fun SC[e:PTag->E, X:Exec_C] : set E { X.SC - e[rm_EV] }

pred wf_s[e:PTag->E, X:Exec_C, s:E->E] { 

  // s is restricted to sc events
  s in SC[e,X] -> SC[e,X]
    
  // s is acylic
  is_acyclic[s]
    
  // s is transitive
  transitive[s]

  // s is a strict total relation on sc events
  (all e1, e2 : (SC[e,X]) | (e1 != e2) iff (e1 -> e2 in (s + ~s)))
}
