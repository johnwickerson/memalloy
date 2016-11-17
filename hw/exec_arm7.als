module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {
  isb : set E, // control fence
  dmb, dmbst, dmbld : set E, // full fences
}{

  // fences must be one (and only one) of the above kinds
  isb + dmb + dmbst + dmbld = F
  disj [isb, dmb, dmbst, dmbld]
    
}
