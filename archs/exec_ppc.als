module exec_ppc[E]
open exec_H[E]

sig Exec_PPC extends Exec_H {
  SYNC, LWSYNC, EIEIO, ISYNC : set E
}{
  SYNC + LWSYNC + EIEIO + ISYNC in F
  disj [ISYNC, LWSYNC + EIEIO]    
  SYNC = LWSYNC & EIEIO  
}

fun ISYNC[e:E, X:Exec_PPC] : set E { X.ISYNC - e }
fun SYNC[e:E, X:Exec_PPC] : set E { X.SYNC - e }
fun LWSYNC[e:E, X:Exec_PPC] : set E { X.LWSYNC - e }
fun EIEIO[e:E, X:Exec_PPC] : set E { X.EIEIO - e }

fun isync[e:E, X:Exec_PPC] : E->E { addsb[e,X,ISYNC[e,X]] }
fun sync[e:E, X:Exec_PPC] : E->E { addsb[e,X,SYNC[e,X]] }
fun lwsync[e:E, X:Exec_PPC] : E->E { addsb[e,X,LWSYNC[e,X]] }
fun eieio[e:E, X:Exec_PPC] : E->E { addsb[e,X,EIEIO[e,X]] }
