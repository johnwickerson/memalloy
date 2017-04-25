module exec_ppc[E]
open exec_H[E]

sig Exec_PPC extends Exec_H {}{}

pred wf_Exec_PPC [X:Exec_PPC, ad,cd,dd,sync,lwsync,eieio,isync:E->E] {

  wf_Exec_H[X,ad,cd,dd]

  is_fence_rel[isync, X.sb]
  is_fence_rel[sync, X.sb]
  is_fence_rel[lwsync, X.sb]
  is_fence_rel[eieio, X.sb]

  // the full fence implies both lightweight fences
  sync in lwsync & eieio  
}

fun isync[e:E, X:Exec_PPC, ad,cd,dd,sync,lwsync,eieio,isync:E->E] : E->E { isync - (univ -> e) - (e -> univ) }
fun sync[e:E, X:Exec_PPC, ad,cd,dd,sync,lwsync,eieio,isync:E->E] : E->E { sync - (univ -> e) - (e -> univ) }
fun lwsync[e:E, X:Exec_PPC, ad,cd,dd,sync,lwsync,eieio,isync:E->E] : E->E { lwsync - (univ -> e) - (e -> univ) }
fun eieio[e:E, X:Exec_PPC, ad,cd,dd,sync,lwsync,eieio,isync:E->E] : E->E { eieio - (univ -> e) - (e -> univ) }

// Synonyms:
fun ISYNC[e:E, X:Exec_PPC,ad,cd,dd,sync,lwsync,eieio,isync_:E->E] : E->E { isync[e,X,ad,cd,dd,sync,lwsync,eieio,isync_] }
fun SYNC[e:E, X:Exec_PPC,ad,cd,dd,sync_,lwsync,eieio,isync:E->E] : E->E { sync[e,X,ad,cd,dd,sync_,lwsync,eieio,isync] }
fun LWSYNC[e:E, X:Exec_PPC,ad,cd,dd,sync,lwsync_,eieio,isync:E->E] : E->E { lwsync[e,X,ad,cd,dd,sync,lwsync_,eieio,isync] }
fun EIEIO[e:E, X:Exec_PPC,ad,cd,dd,sync,lwsync,eieio_,isync:E->E] : E->E { eieio[e,X,ad,cd,dd,sync,lwsync,eieio_,isync] }
