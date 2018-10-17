module exec_x86[E]
open exec_H[E]

sig Exec_X86 extends Exec_H {
  MFENCE : set E, // memory fence
  PFENCE : set E, // persistent fence
  PSYNC : set E   // persistent sync
}{

  // Every fence is an MFENCE
  F = MFENCE

  // No "write back" events
  no WB

  // all locations are persistent
  R + W + WB in PL
		
  // A PFENCE is stronger than an MFENCE	
  PFENCE in MFENCE

  // A PSYNC is stronger than a PFENCE
  PSYNC in PFENCE

  // reads are vacuously persistent
  R in P		

  // nvo relates all and only events that affect non-volatile memory 
  // (i.e. writes, PFENCEs and PSYNCs)
  (nvo + ~nvo) = sq[W + PFENCE] - iden
		
  // RMWs are consecutive
  atom in imm[sb]

  // control dependencies only come out of reads
  cd in (R -> EV)

  // MFENCEs and PSYNCs are always persistent (but PFENCEs may not be) 	
  MFENCE - PFENCE in P
  PSYNC in P
}
 
one sig rm_MFENCE extends PTag {}
one sig rm_PFENCE extends PTag {}
one sig rm_PSYNC extends PTag {}

fun MFENCE[e:PTag->E, X:Exec_X86] : set E {
  X.MFENCE - e[rm_EV] - e[rm_MFENCE] }
fun PFENCE[e:PTag->E, X:Exec_X86] : set E {
  X.PFENCE - e[rm_EV] - e[rm_PFENCE] - e[rm_MFENCE] }
fun PSYNC[e:PTag->E, X:Exec_X86] : set E {
  X.PSYNC - e[rm_EV] - e[rm_PSYNC] - e[rm_PFENCE] - e[rm_MFENCE] }
fun P [e:PTag->E, X:Exec] : set E { X.P - e[rm_EV] }

fun mfence[e:PTag->E, X:Exec_X86] : E->E { addsb[e,X,MFENCE[e,X]] }
fun pfence[e:PTag->E, X:Exec_X86] : E->E { addsb[e,X,PFENCE[e,X]] }
fun psync[e:PTag->E, X:Exec_X86] : E->E { addsb[e,X,PSYNC[e,X]] }
