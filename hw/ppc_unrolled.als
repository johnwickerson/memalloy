module ppc_unrolled[E]
open exec_ppc[E]

fun ctrlisync_[e:E, x : Exec_PPC] : E -> E {
  ^(cd[e,x]) . (stor[isync[e,x]]) . (sb[e,x])
}

fun lwsync_[e:E, x : Exec_PPC] : E -> E {
  (sb[e,x]) . (stor[lwsync[e,x]]) . (sb[e,x])
}

fun sync_[e:E, x : Exec_PPC] : E -> E {
  (sb[e,x]) . (stor[sync[e,x]]) . (sb[e,x])
}

fun eieio_[e:E, x : Exec_PPC] : E -> E {
  (sb[e,x]) . (stor[eieio[e,x]]) . (sb[e,x])
}

fun ppo[e:E, x : Exec_PPC] : E->E {
  let dep = ad[e,x] + dd[e,x] |
  let rdw = poloc[e,x] & ((fre[e,x]) . (rfe[e,x])) |
  let detour = poloc[e,x] & ((coe[e,x]) . (rfe[e,x])) |
  let addrpo = (ad[e,x]) . (sb[e,x]) |

  /* Initial value */  
  let ci0 = (ctrlisync_[e,x]) + detour |
  let ii0 = dep + rfi[e,x] + rdw |
  let cc0 = dep + poloc[e,x] + ^(cd[e,x]) + addrpo |
  let ic0 = none -> none |

  let ci1 = ci0 + ci0.ii0 + cc0.ci0 |
  let ii1 = ii0 + ci0 + ic0.ci0 + ii0.ii0 |
  let cc1 = cc0 + ci0 + ci0.ic0 + cc0.cc0 |
  let ic1 = ic0 + ii0 + cc0 + ic0.cc0 + ii0.ic0 |

  let ci2 = ci0 + ci1.ii1 + cc1.ci1 |
  let ii2 = ii0 + ci1 + ic1.ci1 + ii1.ii1 |
  let cc2 = cc0 + ci1 + ci1.ic1 + cc1.cc1 |
  let ic2 = ic0 + ii1 + cc1 + ic1.cc1 + ii1.ic1 |

  let ci = ci0 + ci2.ii2 + cc2.ci2 |
  let ii = ii0 + ci2 + ic2.ci2 + ii2.ii2 |
  let cc = cc0 + ci2 + ci2.ic2 + cc2.cc2 |
  let ic = ic0 + ii2 + cc2 + ic2.cc2 + ii2.ic2 |

  let ppoR = (R[e,x] -> R[e,x]) & ii |
  let ppoW = (R[e,x] -> W[e,x]) & ic |
  ppoR + ppoW
  
}

fun strong[e:E, x : Exec_PPC] : E -> E {
  sync_[e,x]
}

fun light[e:E, x : Exec_PPC] : E -> E {
  let lwsync = lwsync_[e,x] - (W[e,x] -> R[e,x]) |
  let eieio = eieio_[e,x] & (W[e,x] -> W[e,x])  |
  lwsync + eieio
}

fun fence[e:E, x : Exec_PPC] : E -> E {
  strong[e,x] + light[e,x]
}

fun ppoext[e:E, x : Exec_PPC] : E -> E {
  ((rfe[e,x]) . (ppo[e,x])) + 
               ((ppo[e,x]) . (rfe[e,x])) + 
  ((rfe[e,x]) . (ppo[e,x]) . (rfe[e,x]))
}

fun fenceext[e:E, x : Exec_PPC] : E -> E {
  ((rfe[e,x]) . (fence[e,x])) + 
               ((fence[e,x]) . (rfe[e,x])) + 
  ((rfe[e,x]) . (fence[e,x]) . (rfe[e,x]))
}

fun hb[e:E, x : Exec_PPC] : E -> E {
   ppo[e,x] + fence[e,x] + rfe[e,x]
}

pred No_thin_air[e:E, x : Exec_PPC] {
  is_acyclic[hb[e,x]]
}

fun prop[e:E, x : Exec_PPC] : E -> E {
  let propbase = (fence[e,x] + ((rfe[e,x]) . (fence[e,x]))) . *(hb[e,x]) |		
  let chapo = rfe[e,x] + fre[e,x] + coe[e,x] + 
     ((fre[e,x]) . (rfe[e,x])) + ((coe[e,x]) . (rfe[e,x])) |
  (W[e,x] -> W[e,x]) & propbase + 
  (rc[chapo] . *propbase . (strong[e,x]) . *(hb[e,x]))
}

pred Propagation[e:E, x : Exec_PPC] {
  is_acyclic[co[e,x] + prop[e,x]]
}

pred Observation[e:E, x : Exec_PPC] {
  irreflexive[(fre[e,x]) . (prop[e,x]) . *(hb[e,x])]
}

pred ScXX[e:E, x:Exec_PPC] {
  // the Herd model defines "X" as the set of atomic events
  // but we just derive "X" (aka "atomic") as those events that 
  // are attached to an "atom" edge. 
  let atomic = dom[atom[e,x]] + ran[atom[e,x]] |
  let xx = (sb[e,x]) & (atomic -> atomic) |
  is_acyclic[co[e,x] + xx[e,x]]
}

pred consistent[e:E, x : Exec_PPC] {     
  Uniproc[e,x]	
  Atomic[e,x]
  No_thin_air[e,x]
  Propagation[e,x]
  Observation[e,x]
  ScXX[e,x]
}

run {
  some x : Exec_PPC | loadbuffering_H[x] && consistent[none,x] 
} for exactly 1 Exec, 4 E

run {
  some x : Exec_PPC | storebuffering_H[x] && consistent[none,x] 
} for exactly 1 Exec, 4 E

run {
  some x : Exec_PPC | iriw_H1[x] && consistent[none,x]
} for exactly 1 Exec, 6 E
