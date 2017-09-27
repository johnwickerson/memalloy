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
  (univ - e[rm_EV] - *(X.sb).(e[rm_isync])) <: X.isync :> (univ - e[rm_EV]) }
fun sync[e:PTag->E, X:Exec_PPC] : E->E {
  (univ - e[rm_EV] - *(X.sb).(e[rm_sync] + e[rm_lwsync])) <: X.sync :> (univ - e[rm_EV]) }
fun lwsync[e:PTag->E, X:Exec_PPC] : E->E {
  (univ - e[rm_EV] - *(X.sb).(e[rm_lwsync])) <: X.lwsync :> (univ - e[rm_EV]) }
