module exec_C[E]
open exec[E]

sig Exec_C extends Exec {
  A : set E,            // atomic events
  NAL : set E,          // events accessing non-atomic locations
  ACQ, REL, SC : set E, // acquire, release, sc events
  atxn : E->E,          // atomic transaction
}{

  // initial writes are non-atomic
  A in EV - IW

  // some reads and writes may access "non-atomic" locations
  NAL in (R + W)

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

  // naL contains zero or more sloc-classes
  NAL . sloc = NAL

  // atomic events do not access non-atomic locations
  no (A & NAL)

  // non-atomic reads do not access atomic locations
  R-A in NAL

  // atomic transactions are a type of transaction
  atxn in stxn

  // atxn is a partial equivalence relation among a subset of
  // the non-initalisation events
  atxn in (EV - IW) -> (EV - IW)
  symmetric[atxn]
  transitive[atxn]

  // atomic transactions must be contiguous
  ((sb.sb & atxn) . ~sb) & sb in atxn
    
  // atomic transactions do not contain atomic operations
  no (A & dom[atxn])

  // control dependencies only come out of reads
  cd in (R -> EV)

  // RMWs are consecutive and do not straddle XBEGIN/XEND instructions
  atom in imm[sb]
  atom in stxn + ((EV - dom[stxn]) -> (EV - dom[stxn]))

}

one sig rm_A extends PTag {}
one sig rm_ACQ extends PTag {}
one sig rm_REL extends PTag {}
one sig rm_SC extends PTag {}

fun A[e:PTag->E, X:Exec_C] : set E {
  X.A - e[rm_EV] - e[rm_A] }
fun ACQ[e:PTag->E, X:Exec_C] : set E {
  X.ACQ - e[rm_EV] - e[rm_ACQ] - e[rm_A] }
fun REL[e:PTag->E, X:Exec_C] : set E {
  X.REL - e[rm_EV] - e[rm_REL] - e[rm_A] }
fun SC[e:PTag->E, X:Exec_C] : set E {
  X.SC - e[rm_EV] - e[rm_SC] - e[rm_REL] - e[rm_ACQ] - e[rm_A] }
fun NAL [e:PTag->E, X:Exec] : set E { X.NAL - e[rm_EV] }
    
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
