/* Manually generated from ppc.cat */

module ppc[E]
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

pred valid_ppo[e:E, x : Exec_PPC, ppo : E -> E] {
  let dep = ad[e,x] + dd[e,x] |
  let rdw = poloc[e,x] & ((fre[e,x]) . (rfe[e,x])) |
  let detour = poloc[e,x] & ((coe[e,x]) . (rfe[e,x])) |
  let addrpo = (ad[e,x]) . (sb[e,x]) |

  /* Initial value */  
  let ci0 = (ctrlisync_[e,x]) + detour |
  let ii0 = dep + rfi[e,x] + rdw |
  let cc0 = dep + poloc[e,x] + ^(cd[e,x]) + addrpo |
  let ic0 = none -> none |

  some ci,ii,cc,ic : E -> E {
    ci0 + ci.ii + cc.ci in ci
    ii0 + ci + ic.ci + ii.ii in ii
    cc0 + ci + ci.ic + cc.cc in cc
    ic0 + ii + cc + ic.cc + ii.ic in ic
    all ci', ii', cc', ic' : E -> E | 
      (ci0 + ci'.ii' + cc'.ci' in ci' &&
      ii0 + ci' + ic'.ci' + ii'.ii' in ii' &&
      cc0 + ci' + ci'.ic' + cc'.cc' in cc' &&
      ic0 + ii' + cc' + ic'.cc' + ii.ic' in ic') 
    implies
      (ci in ci' && ii in ii' && cc in cc' && ic in ic')
    let ppoR = (R[e,x] -> R[e,x]) & ii |
    let ppoW = (R[e,x] -> W[e,x]) & ic |
    ppo = ppoR + ppoW
  }
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

fun ppoext[e:E, x : Exec_PPC, ppo : E -> E] : E -> E {
  ((rfe[e,x]) . ppo) + 
               (ppo . (rfe[e,x])) + 
  ((rfe[e,x]) . ppo . (rfe[e,x]))
}

fun fenceext[e:E, x : Exec_PPC] : E -> E {
  ((rfe[e,x]) . (fence[e,x])) + 
               ((fence[e,x]) . (rfe[e,x])) + 
  ((rfe[e,x]) . (fence[e,x]) . (rfe[e,x]))
}

fun hb[e:E, x : Exec_PPC, ppo : E -> E] : E -> E {
   ppo + fence[e,x] + rfe[e,x]
}

pred No_thin_air[e:E, x : Exec_PPC, ppo : E -> E] {
  is_acyclic[hb[e,x,ppo]]
}

fun prop[e:E, x : Exec_PPC, ppo: E -> E] : E -> E {
  let propbase = (fence[e,x] + ((rfe[e,x]) . (fence[e,x]))) .
     *(hb[e,x,ppo]) |		
  let chapo = rfe[e,x] + fre[e,x] + coe[e,x] + 
     ((fre[e,x]) . (rfe[e,x])) + ((coe[e,x]) . (rfe[e,x])) |
  (W[e,x] -> W[e,x]) & propbase + 
  (rc[chapo] . *propbase . (strong[e,x]) . *(hb[e,x,ppo]))
}

pred Propagation[e:E, x : Exec_PPC, ppo : E -> E] {
  is_acyclic[co[e,x] + prop[e,x,ppo]]
}

pred Observation[e:E, x : Exec_PPC, ppo : E -> E] {
  irreflexive[(fre[e,x]) . (prop[e,x,ppo]) . *(hb[e,x,ppo])]
}

pred ScXX[e:E, x:Exec_PPC] {
  // the Herd model defines "X" as the set of atomic events
  // but we just derive "X" (aka "atomic") as those events that 
  // are attached to an "atom" edge. 
  let atomic = dom[atom[e,x]] + ran[atom[e,x]] |
  let xx = (sb[e,x]) & (atomic -> atomic) |
  is_acyclic[co[e,x] + xx]
}

pred consistent[e:E, x : Exec_PPC] {     
  Uniproc[e,x]
  Atomic[e,x]
  some ppo : E -> E {
    valid_ppo[e,x,ppo]
    No_thin_air[e,x,ppo]
    Propagation[e,x,ppo]
    Observation[e,x,ppo]
  }
  ScXX[e,x]
}

run {
  some x : Exec_PPC | loadbuffering_H[x] && consistent[none, x] 
} for exactly 1 Exec, 4 E

run {
  some x : Exec_PPC | storebuffering_H[x] && consistent[none, x] 
} for exactly 1 Exec, 4 E

run {
  some x : Exec_PPC | iriw_H1[x] && consistent[none, x]
} for exactly 1 Exec, 6 E
