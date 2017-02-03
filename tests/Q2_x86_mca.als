open ../models_als/x86tso[E] as M1

sig E {}

pred mca_consistent[e:E, x:Exec] {
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

run gp for 1 Exec, 8 E, 5 Int expect 0 // 2 min (glucose, benjamin)
run gp for 1 Exec, 9 E, 5 Int expect 0 // 17 min (glucose, benjamin)
