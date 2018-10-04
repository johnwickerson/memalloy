module exec_C[E]
open exec[E]

sig Exec_C extends Exec {
  WPF : set E,          // weak persistent barrier events
  PF : set E,           // persistent barrier events
  PS : set E,           // persistent sync events
  A : set E,            // atomic events
  NAL : set E,          // events accessing non-atomic locations
  ACQ, REL, SC : set E, // acquire, release, sc events
  P : set E,            // persisted events
  PL : set E,           // events accessing persistent locations
  nvo : E->E,           // non-volatile order
}{
  
  // initial writes are non-atomic
  A in EV - IW

  // initial writes to persistent locations persist
  IW & PL in P

  // some reads and writes may access "non-atomic" locations
  NAL in (R + W)

  // some reads and writes may access persistent locations		
  PL in (R + W)
		
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

  // a weak persistent barrier is a type of fence
  WPF in F

  // a persistent barrier is at least as strong as a weak persistent fence
  PF in WPF

  // a persistent sync is at least as strong as a persistent barrier		
  PS in PF
		
  // naL contains zero or more sloc-classes
  NAL . sloc = NAL

  // PL contains zero or more sloc-classes
  PL . sloc = PL		

  // atomic events do not access non-atomic locations
  no (A & NAL)

  // non-atomic reads do not access atomic locations
  R-A in NAL

  // control dependencies only come out of reads
  cd in (R -> EV)

  // RMWs are consecutive
  atom in imm[sb]

  // nvo relates all and only durable events
  strict_partial_order[nvo]
  let D = (W & PL) + WB + PF |
  (nvo + ~nvo) = sq[D] - iden		

  // nvo is prefix-closed with respect to persistent events
  (nvo . P) in P

  // persistent syncs and write-backs are always persistent
  PS + WB in P
		
}

one sig rm_A extends PTag {}
one sig rm_ACQ extends PTag {}
one sig rm_REL extends PTag {}
one sig rm_SC extends PTag {}
one sig rm_WPF extends PTag {}
one sig rm_PF extends PTag {}
one sig rm_PS extends PTag {}

fun A[e:PTag->E, X:Exec_C] : set E {
  X.A - e[rm_EV] - e[rm_A] }
fun ACQ[e:PTag->E, X:Exec_C] : set E {
  X.ACQ - e[rm_EV] - e[rm_ACQ] - e[rm_A] }
fun REL[e:PTag->E, X:Exec_C] : set E {
  X.REL - e[rm_EV] - e[rm_REL] - e[rm_A] }
fun SC[e:PTag->E, X:Exec_C] : set E {
  X.SC - e[rm_EV] - e[rm_SC] - e[rm_REL] - e[rm_ACQ] - e[rm_A] }
fun NAL [e:PTag->E, X:Exec] : set E { X.NAL - e[rm_EV] }
fun PL [e:PTag->E, X:Exec] : set E { X.PL - e[rm_EV] }    
fun P [e:PTag->E, X:Exec] : set E { X.P - e[rm_EV] }  
fun WPF [e:PTag->E, X:Exec] : set E { X.WPF - e[rm_EV] - e[rm_WPF] }  
fun PF [e:PTag->E, X:Exec] : set E { X.PF - e[rm_EV] - e[rm_WPF] - e[rm_PF] } 
fun PS [e:PTag->E, X:Exec] : set E { X.PS - e[rm_EV] - e[rm_WPF] - e[rm_PF] - e[rm_PS] } 

fun nvo [e:PTag->E, X:Exec] : E->E { rm_EV_rel[e, X.nvo] }

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
