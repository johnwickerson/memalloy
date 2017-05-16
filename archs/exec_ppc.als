module exec_ppc[E]
open exec_H[E]

sig Exec_PPC extends Exec_H {
  SYNC, LWSYNC, EIEIO, ISYNC : set E
}{
  SYNC + LWSYNC + EIEIO + ISYNC in F
  disj [ISYNC, LWSYNC + EIEIO]    
  SYNC = LWSYNC & EIEIO  
}

fun ISYNC[e:PTag->E, X:Exec_PPC] : set E { X.ISYNC - e[rm_EV] }
fun SYNC[e:PTag->E, X:Exec_PPC] : set E { X.SYNC - e[rm_EV] }
fun LWSYNC[e:PTag->E, X:Exec_PPC] : set E { X.LWSYNC - e[rm_EV] }
fun EIEIO[e:PTag->E, X:Exec_PPC] : set E { X.EIEIO - e[rm_EV] }

fun isync[e:PTag->E, X:Exec_PPC] : E->E { addsb[e,X,ISYNC[e,X]] }
fun sync[e:PTag->E, X:Exec_PPC] : E->E { addsb[e,X,SYNC[e,X]] }
fun lwsync[e:PTag->E, X:Exec_PPC] : E->E { addsb[e,X,LWSYNC[e,X]] }
fun eieio[e:PTag->E, X:Exec_PPC] : E->E { addsb[e,X,EIEIO[e,X]] }
