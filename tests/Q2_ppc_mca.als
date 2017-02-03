open ../models_als/ppc_unrolled[E] as M1

sig E {}

pred mca_consistent[e:E,x:Exec] {
  is_acyclic[(sb[e,x] & sloc[e,x]) + co[e,x]]
  is_acyclic[wo[e,x]]
}

fun wo[e:E, x:Exec] : E->E {
  ((((rfe[e,x]) . (M1/ppo[e,x]) . ~(rfe[e,x])) - iden) . (co[e,x]))
  + ((rfe[e,x]) . (M1/ppo[e,x]) . (fr_init[e,x]))
}

pred gp [X:Exec_H] {
  withoutinit[X]
  M1/consistent[none,X]
  not (mca_consistent[none,X])
}

run gp for 1 Exec, 5 E, 5 Int expect 0 // 2s (glucose, babillion)
run gp for 1 Exec, 6 E, 5 Int expect 1 // 2s (glucose, babillion)
// (Alloy finds exactly IRIW+addrs)
