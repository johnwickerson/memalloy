module exec_C[E]
open exec[E]

sig Exec_C extends Exec {
  ATO : set E,            // atomic events
  ACQ, REL, SC : set E, // acquire, release, sc events
}{

  // initial writes are non-atomic
  ATO in ev - IW

  // acquires, releases, and SC operations are all atomic
  ACQ + REL + SC in ATO

  // RMWs and fences are atomic
  (F + (R & W)) in ATO
    
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
  no (ATO & NAL)

  // non-atomic reads do not access atomic locations
  R-ATO in NAL

}

fun ATO[e:E, X:Exec_C] : set E { X.ATO - e }
fun ACQ[e:E, X:Exec_C] : set E { X.ACQ - e }
fun REL[e:E, X:Exec_C] : set E { X.REL - e }
fun SC[e:E, X:Exec_C] : set E { X.SC - e }

pred wf_s[e:E, X:Exec_C, s:E->E] { 

  // s is restricted to sc events
  s in SC[e,X] -> SC[e,X]
    
  // s is acylic
  is_acyclic[s]
    
  // s is transitive
  transitive[s]

  // s is a strict total relation on sc events
  (all e1, e2 : (SC[e,X]) | (e1 != e2) iff (e1 -> e2 in (s + ~s)))
}
