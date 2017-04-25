module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {
  isb, dmbst, dmbld, dmb : set E
}{
  isb + dmbst + dmbld + dmb in F
  disj[isb, dmbst + dmbld]
  dmb = dmbst & dmbld
}

fun isb[e:E, X:Exec_Arm7] : set E { X.isb - e }
fun dmbst[e:E, X:Exec_Arm7] : set E { X.dmbst - e }
fun dmbld[e:E, X:Exec_Arm7] : set E { X.dmbld - e }
fun dmb[e:E, X:Exec_Arm7] : set E { X.dmb - e }

// Synonyms:
fun ISB[e:E, X:Exec_Arm7] : set E { isb[e,X] }
fun DMB[e:E, X:Exec_Arm7] : set E { dmb[e,X] }
fun DSB[e:E, X:Exec_Arm7] : set E { dmb[e,X] } // dsb = dmb
fun DMBSY[e:E, X:Exec_Arm7] : set E { dmb[e,X] }
fun DMBST[e:E, X:Exec_Arm7] : set E { dmbst[e,X] }
fun DMBLD[e:E, X:Exec_Arm7] : set E { dmbld[e,X] }
fun DSBST[e:E, X:Exec_Arm7] : set E { dmbst[e,X] } // dsb = dmb
