module ppc0[E]
open exec_ppc[E]

fun ctrlisync_[x : Exec_PPC] : E -> E {
  ^(x.cd) . (stor[x.isync]) . (x.sb)
}

fun lwsync_[x : Exec_PPC] : E -> E {
  (x.sb) . (stor[x.lwsync]) . (x.sb)
}

fun sync_[x : Exec_PPC] : E -> E {
  (x.sb) . (stor[x.sync]) . (x.sb)
}

fun eieio_[x : Exec_PPC] : E -> E {
  (x.sb) . (stor[x.eieio]) . (x.sb)
}

fun ppo[x : Exec_PPC] : E->E {
  let dep = x.ad + x.dd |
  let rdw = poloc[x] & ((fre[x]) . (rfe[x])) |
  let detour = poloc[x] & ((coe[x]) . (rfe[x])) |
  let addrpo = (x.ad) . (x.sb) |

  /* Initial value */  
  let ci = (ctrlisync_[x]) + detour |
  let ii = dep + rfi[x] + rdw |
  let cc = dep + poloc[x] + ^(x.cd) + addrpo |
  let ic = none -> none |

  let ppoR = (x.R -> x.R) & ii |
  let ppoW = (x.R -> x.W) & ic |
  ppoR + ppoW
  
}

fun strong[x : Exec_PPC] : E -> E {
  sync_[x]
}

fun light[x : Exec_PPC] : E -> E {
  let lwsync = lwsync_[x] - (x.W -> x.R) |
  let eieio = eieio_[x] & (x.W -> x.W)  |
  lwsync + eieio
}

fun fence[x : Exec_PPC] : E -> E {
  strong[x] + light[x]
}

fun ppoext[x : Exec_PPC] : E -> E {
  ((rfe[x]) . (ppo[x])) + 
             ((ppo[x]) . (rfe[x])) + 
  ((rfe[x]) . (ppo[x]) . (rfe[x]))
}

fun fenceext[x : Exec_PPC] : E -> E {
  ((rfe[x]) . (fence[x])) + 
             ((fence[x]) . (rfe[x])) + 
  ((rfe[x]) . (fence[x]) . (rfe[x]))
}

fun hb[x : Exec_PPC] : E -> E {
   ppo[x] + fence[x] + rfe[x]
}

pred No_thin_air[x : Exec_PPC] {
  is_acyclic[hb[x]]
}

fun prop[x : Exec_PPC] : E -> E {
  let propbase = (fence[x] + ((rfe[x]) . (fence[x]))) . *(hb[x]) |		
  let chapo = rfe[x] + fre[x] + coe[x] + 
     ((fre[x]) . (rfe[x])) + ((coe[x]) . (rfe[x])) |
  (x.W -> x.W) & propbase + 
  (rc[chapo] . *propbase . (strong[x]) . *(hb[x]))
}

pred Propagation[x : Exec_PPC] {
  is_acyclic[x.co + prop[x]]
}

pred Observation[x : Exec_PPC] {
  irreflexive[(fre[x]) . (prop[x]) . *(hb[x])]
}

pred ScXX[x:Exec_PPC] {
  // the Herd model defines "X" as the set of atomic events
  // but we just derive "X" (aka "atomic") as those events that 
  // are attached to an "atom" edge. 
  let atomic = dom[x.atom] + ran[x.atom] |
  let xx = (x.sb) & (atomic -> atomic) |
  is_acyclic[x.co + xx]
}

pred consistent[x : Exec_PPC] {     
  Uniproc[x]	
  Atomic[x]
  Propagation[x]
  Observation[x]
  ScXX[x]
}

run {
  some x : Exec_PPC | loadbuffering_H[x] && consistent[x] 
} for exactly 1 Exec, 4 E

run {
  some x : Exec_PPC | storebuffering_H[x] && consistent[x] 
} for exactly 1 Exec, 4 E

run {
  some x : Exec_PPC | iriw_H1[x] && consistent[x]
} for exactly 1 Exec, 6 E
