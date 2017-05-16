module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {
  ISB, DMBST, DMBLD, DMB : set E
}{
  ISB + DMBST + DMBLD + DMB in F
  disj[ISB, DMBST + DMBLD]
  DMB = DMBST & DMBLD
}

fun ISB[e:PTag->E, X:Exec_Arm7] : set E { X.ISB - e[rm_EV] }
fun DMBST[e:PTag->E, X:Exec_Arm7] : set E { X.DMBST - e[rm_EV] }
fun DMBLD[e:PTag->E, X:Exec_Arm7] : set E { X.DMBLD - e[rm_EV] }
fun DMB[e:PTag->E, X:Exec_Arm7] : set E { X.DMB - e[rm_EV] }

fun isb[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,ISB[e,X]] }
fun dmbst[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMBST[e,X]] }
fun dmbld[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMBLD[e,X]] }
fun dmb[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMB[e,X]] }
