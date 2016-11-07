open ../hw/x86tso[E] as M1

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
  M1/consistent[X]
  not (mca_consistent[X])
}

run gp for 1 Exec, 8 E, 5 Int expect 0 // 2 min (glucose, benjamin)
run gp for 1 Exec, 9 E, 5 Int expect 0 // 17 min (glucose, benjamin)
