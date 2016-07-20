module exec_arm[E]
open ../exec_H[E]

sig Exec_Arm extends Exec_H {
  isb : set E, // control fence
  dmb, dsb, dmbst, dsbst : set E, // full fences
}{

  // fences must be one (and only one) of the above kinds
  isb + dmb + dsb + dmbst + dsbst = F
  disj [isb, dmb, dsb, dmbst, dsbst]
    
}
