module exec_x86[E]
open exec_H[E]

sig Exec_X86 extends Exec_H {
  locked : set E // atomic events
}{
  // only reads and atomic writes can be locked
  locked in R + (W & (univ.atom + atom.univ))
}
