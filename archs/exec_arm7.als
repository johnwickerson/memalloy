module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {
  ISB, DMBST, DMBLD, DMB : set E
}{
  ISB + DMBST + DMBLD + DMB in F
  disj[ISB, DMBST + DMBLD]
  DMB = DMBST & DMBLD
}

fun ISB[e:E, X:Exec_Arm7] : set E { X.ISB - e }
fun DMBST[e:E, X:Exec_Arm7] : set E { X.DMBST - e }
fun DMBLD[e:E, X:Exec_Arm7] : set E { X.DMBLD - e }
fun DMB[e:E, X:Exec_Arm7] : set E { X.DMB - e }

fun isb[e:E, X:Exec_Arm7] : E->E { addsb[e,X,ISB[e,X]] }
fun dmbst[e:E, X:Exec_Arm7] : E->E { addsb[e,X,DMBST[e,X]] }
fun dmbld[e:E, X:Exec_Arm7] : E->E { addsb[e,X,DMBLD[e,X]] }
fun dmb[e:E, X:Exec_Arm7] : E->E { addsb[e,X,DMB[e,X]] }
