module exec_x86[E]
open exec_H[E]

sig Exec_X86 extends Exec_H {
  mfence : E -> E, // memory fence
}{
  is_fence_rel[mfence, sb]

  // RMWs are consecutive
  atom in imm[sb]

  // control dependencies only come out of reads
  cd in (R -> EV)
}

one sig rm_mfence extends PTag {}

fun mfence[e:PTag->E, X:Exec_X86] : E->E {
  mk_fence_rel[e, rm_mfence, X.mfence, X.sb] }
