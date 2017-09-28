module exec_ppc[E]
open exec_H[E]

sig Exec_PPC extends Exec_H {
  sync, lwsync, isync:E->E
}{
  is_fence_rel[isync, sb]
  is_fence_rel[sync, sb]
  is_fence_rel[lwsync, sb]

  // the full fence implies the lightweight fence
  sync in lwsync 
}

one sig rm_isync extends PTag {}
one sig rm_sync extends PTag {}
one sig rm_lwsync extends PTag {}

fun isync[e:PTag->E, X:Exec_PPC] : E->E {
  mk_fence_rel[e, rm_isync, X.isync, X.sb] }

fun sync[e:PTag->E, X:Exec_PPC] : E->E {
  mk_fence_rel[e, rm_sync + rm_lwsync, X.sync, X.sb] }
    
fun lwsync[e:PTag->E, X:Exec_PPC] : E->E {
  mk_fence_rel[e, rm_lwsync, X.lwsync, X.sb] }
