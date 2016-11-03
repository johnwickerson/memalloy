module exec_x86[E]
open exec_H[E]

sig Exec_X86 extends Exec_H {
  locked : set E // atomic events
}{
  // only RMWs can be locked
  locked in univ.atom + atom.univ

  // the atom relation only relates locked instructions
  atom in (locked -> locked)
}
