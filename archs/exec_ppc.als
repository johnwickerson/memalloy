module exec_ppc[E]
open exec_H[E]

sig Exec_PPC extends Exec_H {
  SYNC, LWSYNC, ISYNC : set E
}{
  LWSYNC + ISYNC = F
  disj [ISYNC, LWSYNC]    
  SYNC in LWSYNC  
}

one sig rm_SYNC extends PTag {}

fun ISYNC[e:PTag->E, X:Exec_PPC] : set E {
  X.ISYNC - e[rm_EV] }
fun SYNC[e:PTag->E, X:Exec_PPC] : set E {
  X.SYNC - e[rm_EV] - e[rm_SYNC] }
fun LWSYNC[e:PTag->E, X:Exec_PPC] : set E {
  X.LWSYNC - e[rm_EV] }

fun isync[e:PTag->E, X:Exec_PPC] : E->E { addsb[e,X,ISYNC[e,X]] }
fun sync[e:PTag->E, X:Exec_PPC] : E->E { addsb[e,X,SYNC[e,X]] }
fun lwsync[e:PTag->E, X:Exec_PPC] : E->E { addsb[e,X,LWSYNC[e,X]] }
