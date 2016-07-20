
module prog_x86
open ../../prog

sig Prog_x86 extends Prog {
  insts_locked : set Inst, // locked instructions
} {

  // the locked instructions are a subset of the reachable instructions
  insts_locked in top.*child

}

