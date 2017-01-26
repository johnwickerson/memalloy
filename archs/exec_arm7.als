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

fun isb[e:E, X:Exec_Arm7] : set E { X.isb - e }
fun dmb[e:E, X:Exec_Arm7] : set E { X.dmb - e }
fun dmbst[e:E, X:Exec_Arm7] : set E { X.dmbst - e }
fun dmbld[e:E, X:Exec_Arm7] : set E { X.dmbld - e }
