open ../../hw/cpu/ppc2[E] as M1

sig E {}

pred mca_consistent[x:Exec] {
  is_acyclic[(x.sb & x.sloc) + x.co]
  is_acyclic[wo[x]]
}

fun wo[x:Exec] : E->E {
  ((((rfe[x]) . (M1/ppo[x]) . ~(rfe[x])) - iden) . (x.co))
  + ((rfe[x]) . (M1/ppo[x]) . (fr_init[x]))
}

pred gp [X:Exec_H] { 
  withoutinit[X]
  M1/consistent[X]
  not (mca_consistent[X])
}

run gp for 1 Exec, 5 E, 5 Int expect 0 // 2s (glucose, babillion)
run gp for 1 Exec, 6 E, 5 Int expect 1 // 2s (glucose, babillion)
// (Alloy finds exactly IRIW+addrs)
