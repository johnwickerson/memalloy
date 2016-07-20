module exec_x86[E]
open ../exec_H[E]

sig Exec_X86 extends Exec_H {
  locked : set E // atomic events
}{
  // only reads and non-initial writes can be locked
  locked in (R + W) - I
}
