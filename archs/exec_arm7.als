module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {
  	ISB, DMBST, DMBLD: set E,
	DSB   : set E, // data synchronisation barrier
}{
  ISB + DMBST + DMBLD = F
  disj[ISB, DMBST + DMBLD]
  //NB: DMB = DMBST & DMBLD

  // a DSB is strictly stronger than a DMB
  DSB in DMBST & DMBLD
		
  // control dependencies only come out of reads
  cd in (R -> EV)

  // nvo relates all and only durable events
  let D = (W & PL) + WB {
  (nvo + ~nvo) = sq[D] - iden
  P in D	  	  	
  }

  // write-backs are always persistent
  WB in P
		
}

one sig rm_DMBST extends PTag {}
one sig rm_DMBLD extends PTag {}
one sig rm_DSB extends PTag {}

fun ISB[e:PTag->E, X:Exec_Arm7] : set E {
  X.ISB - e[rm_EV] }
fun DMBST[e:PTag->E, X:Exec_Arm7] : set E {
  X.DMBST - e[rm_EV] - e[rm_DMBST] }
fun DMBLD[e:PTag->E, X:Exec_Arm7] : set E {
  X.DMBLD - e[rm_EV] - e[rm_DMBLD] }
fun DMB[e:PTag->E, X:Exec_Arm7] : set E {
  DMBLD[e,X] & DMBST[e,X] }
fun DSB [e:PTag->E, X:Exec_Arm7] : set E { X.DSB - e[rm_EV] - e[rm_DMBLD] - e[rm_DMBST] }
    
fun isb[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,ISB[e,X]] }
fun dmbst[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMBST[e,X]] }
fun dmbld[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMBLD[e,X]] }
fun dmb[e:PTag->E, X:Exec_Arm7] : E->E { addsb[e,X,DMB[e,X]] }
