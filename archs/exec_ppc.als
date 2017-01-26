module exec_ppc[E]
open exec_H[E]

sig Exec_PPC extends Exec_H {
  isync : set E, // control fence
  sync : set E, // full fence
  lwsync, eieio : set E, // lightweight fences
}{

  // fences must be one (and only one) of the above kinds
  isync + sync + lwsync + eieio = F
  disj [isync, sync, lwsync, eieio]
}

fun isync[e:E, X:Exec_PPC] : set E { X.isync - e }
fun sync[e:E, X:Exec_PPC] : set E { X.sync - e }
fun lwsync[e:E, X:Exec_PPC] : set E { X.lwsync - e }
fun eieio[e:E, X:Exec_PPC] : set E { X.eieio - e }

// Synonyms:
fun ISYNC[e:E, X:Exec_PPC] : set E { isync[e,X] }
fun SYNC[e:E, X:Exec_PPC] : set E { sync[e,X] }
fun LWSYNC[e:E, X:Exec_PPC] : set E { lwsync[e,X] }
fun EIEIO[e:E, X:Exec_PPC] : set E { eieio[e,X] }
