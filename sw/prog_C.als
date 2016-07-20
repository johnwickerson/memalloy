module prog_C
open ../prog

sig Prog_C extends Prog {
  insts_atomic : set Inst, // atomic instructions
  insts_acq : set Inst, // acquire instructions
  insts_rel : set Inst, // release instructions
  insts_sc : set Inst // seq_cst instructions
} {

  // the atomic instructions are a subset of the reachable instructions
  insts_atomic in cm

  // exchanges and fences are atomic
  cm & (Cas + Fence) in insts_atomic

  // memory orders are used correctly
  insts_acq + insts_rel in insts_atomic
  insts_acq in Load + Cas + Fence
  insts_rel in Store + Cas + Fence
  insts_sc in insts_acq + insts_rel

}

