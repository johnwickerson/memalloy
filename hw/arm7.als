/* Manually generated from arm.cat */

module arm7[E]
open exec_arm7[E]

fun ctrlisb_[e:E, x : Exec_H] : E -> E {
  ^(cd[e,x]) . (stor[isb[e,x]]) . (sb[e,x])
}

fun dmb_[e:E, x : Exec_H] : E -> E {
  (sb[e,x]) . (stor[dmb[e,x]]) . (sb[e,x])
}

fun dmbst_[e:E, x : Exec_H] : E -> E {
  (sb[e,x]) . (stor[dmbst[e,x]]) . (sb[e,x])
}

pred valid_ppo[e:E, x : Exec_H, ppo : E -> E] {
  let dep = ad[e,x] + dd[e,x] |
  let rdw = poloc[e,x] & ((fre[e,x]) . (rfe[e,x])) |
  let detour = poloc[e,x] & ((coe[e,x]) . (rfe[e,x])) |
  let addrpo = (ad[e,x]) . (sb[e,x]) |

  /* Initial value */  
  let ci0 = (ctrlisb_[e,x]) + detour |
  let ii0 = dep + rfi[e,x] + rdw |
  let cc0 = dep /*+ poloc[x]*/ + ^(cd[e,x]) + addrpo |
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

fun strong[e:E, x : Exec_H] : E -> E {
  dmb_[e,x] + dmbst_[e,x]
}

fun light[e:E, x : Exec_H] : E -> E {
  none -> none
}

fun fence[e:E, x : Exec_H] : E -> E {
  strong[e,x] + light[e,x]
}

fun hb[e:E, x : Exec_H, ppo : E -> E] : E -> E {
   ppo + fence[e,x] + rfe[e,x]
}

pred No_thin_air[e:E, x : Exec_H, ppo : E -> E] {
  is_acyclic[hb[e,x,ppo]]
}

fun prop[e:E, x : Exec_H, ppo: E -> E] : E -> E {
  let propbase = (fence[e,x] + (rfe[e,x]) . (fence[e,x])) . 
     *(hb[e,x,ppo]) |		
  let chapo = rfe[e,x] + fre[e,x] + coe[e,x] + 
     ((fre[e,x]) . (rfe[e,x])) + ((coe[e,x]) . (rfe[e,x])) |
  (W[e,x] -> W[e,x]) & propbase + 
  (rc[chapo] . *propbase . (strong[e,x]) . *(hb[e,x,ppo]))
}

pred Propagation[e:E, x : Exec_H, ppo : E -> E] {
  is_acyclic[co[e,x] + prop[e,x,ppo]]
}

pred Causality[e:E, x : Exec_H, ppo : E -> E] {
  irreflexive[(fre[e,x]) . (prop[e,x,ppo]) . *(hb[e,x,ppo])]
}


pred consistent[e:E, x : Exec_H] {     
  Uniproc[e,x]	
  some ppo : E -> E {
    valid_ppo[e,x,ppo]
    No_thin_air[e,x,ppo]
    Propagation[e,x,ppo]
    Causality[e,x,ppo]
  }
}
