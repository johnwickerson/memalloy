module exec_x86[E]
open exec_H[E]

sig Exec_X86 extends Exec_H {
  MFENCE : set E // memory fence
}{
  MFENCE = F

  // RMWs are consecutive and do not straddle XBEGIN/XEND instructions
  atom in imm[sb]
  atom in stxn + ((EV - dom[stxn]) -> (EV - dom[stxn]))
}

one sig rm_MFENCE extends PTag {}

fun MFENCE[e:PTag->E, X:Exec_X86] : set E {
  X.MFENCE - e[rm_EV] - e[rm_MFENCE] }

fun mfence[e:PTag->E, X:Exec_X86] : E->E { addsb[e,X,MFENCE[e,X]] }
