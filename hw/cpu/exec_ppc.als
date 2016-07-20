module exec_ppc[E]
open ../exec_H[E]

sig Exec_PPC extends Exec_H {
  isync : set E, // control fence
  sync : set E, // full fence
  lwsync, eieio : set E, // lightweight fences
}{

  // fences must be one (and only one) of the above kinds
  isync + sync + lwsync + eieio = F
  disj [isync, sync, lwsync, eieio]
}
