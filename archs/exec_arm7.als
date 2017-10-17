module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {
  ISB, DMBST, DMBLD, DMB : set E
}{
  ISB + DMBST + DMBLD + DMB = F
  disj[ISB, DMBST + DMBLD]
  DMB in DMBST & DMBLD

  // control dependencies only come out of reads
  cd in (R -> EV)
}

one sig rm_ISB extends PTag {}
one sig rm_DMBST extends PTag {}
one sig rm_DMBLD extends PTag {}
one sig rm_DMB extends PTag {}

fun ISB[e:PTag->E, X:Exec_Arm7] : set E {
  X.ISB - e[rm_EV] - e[rm_ISB] }
fun DMBST[e:PTag->E, X:Exec_Arm7] : set E {
  X.DMBST - e[rm_EV] - e[rm_DMBST] }
fun DMBLD[e:PTag->E, X:Exec_Arm7] : set E {
  X.DMBLD - e[rm_EV] - e[rm_DMBLD] }
fun DMB[e:PTag->E, X:Exec_Arm7] : set E {
  X.DMB - e[rm_EV] - e[rm_DMB] - e[rm_DMBST] - e[rm_DMBLD] }

fun isb[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,ISB[e,X]] }
fun dmbst[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMBST[e,X]] }
fun dmbld[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMBLD[e,X]] }
fun dmb[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMB[e,X]] }
