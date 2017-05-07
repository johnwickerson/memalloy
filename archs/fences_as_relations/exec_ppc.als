module exec_ppc[E]
open exec_H[E]

sig Exec_PPC extends Exec_H {
  sync, lwsync, eieio, isync:E->E
}{
  is_fence_rel[isync, sb]
  is_fence_rel[sync, sb]
  is_fence_rel[lwsync, sb]
  is_fence_rel[eieio, sb]

  // the full fence implies both lightweight fences
  sync in lwsync & eieio 
}

fun isync[e:E, X:Exec_PPC] : E->E { X.isync - (univ -> e) - (e -> univ) }
fun sync[e:E, X:Exec_PPC] : E->E { X.sync - (univ -> e) - (e -> univ) }
fun lwsync[e:E, X:Exec_PPC] : E->E { X.lwsync - (univ -> e) - (e -> univ) }
fun eieio[e:E, X:Exec_PPC] : E->E { X.eieio - (univ -> e) - (e -> univ) }
