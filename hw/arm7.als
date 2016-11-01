/* Manually generated from arm.cat */

module arm7[E]
open exec_arm7[E]

fun ctrlisb_[x : Exec_H] : E -> E {
  ^(x.cd) . (stor[x.isb]) . (x.sb)
}

fun dsb_[x : Exec_H] : E -> E {
  (x.sb) . (stor[x.dsb]) . (x.sb)
}

fun dmb_[x : Exec_H] : E -> E {
  (x.sb) . (stor[x.dmb]) . (x.sb)
}

fun dsbst_[x : Exec_H] : E -> E {
  (x.sb) . (stor[x.dsbst]) . (x.sb)
}

fun dmbst_[x : Exec_H] : E -> E {
  (x.sb) . (stor[x.dmbst]) . (x.sb)
}

pred valid_ppo[x : Exec_H, ppo : E -> E] {
  let dep = x.ad + x.dd |
  let rdw = poloc[x] & ((fre[x]) . (rfe[x])) |
  let detour = poloc[x] & ((coe[x]) . (rfe[x])) |
  let addrpo = (x.ad) . (x.sb) |

  /* Initial value */  
  let ci0 = (ctrlisb_[x]) + detour |
  let ii0 = dep + rfi[x] + rdw |
  let cc0 = dep /*+ poloc[x]*/ + ^(x.cd) + addrpo |
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
    let ppoR = (x.R -> x.R) & ii |
    let ppoW = (x.R -> x.W) & ic |
    ppo = ppoR + ppoW
  }
}

fun strong[x : Exec_H] : E -> E {
  dsb_[x] + dmb_[x] + dsbst_[x] + dmbst_[x]
}

fun light[x : Exec_H] : E -> E {
  none -> none
}

fun fence[x : Exec_H] : E -> E {
  strong[x] + light[x]
}

fun hb[x : Exec_H, ppo : E -> E] : E -> E {
   ppo + fence[x] + rfe[x]
}

pred No_thin_air[x : Exec_H, ppo : E -> E] {
  is_acyclic[hb[x,ppo]]
}

fun prop[x : Exec_H, ppo: E -> E] : E -> E {
  let propbase = (fence[x] + (rfe[x]) . (fence[x])) . 
     *(hb[x,ppo]) |		
  let chapo = rfe[x] + fre[x] + coe[x] + 
     ((fre[x]) . (rfe[x])) + ((coe[x]) . (rfe[x])) |
  (x.W -> x.W) & propbase + 
  (rc[chapo] . *propbase . (strong[x]) . *(hb[x,ppo]))
}

pred Propagation[x : Exec_H, ppo : E -> E] {
  is_acyclic[co + prop[x,ppo]]
}

pred Causality[x : Exec_H, ppo : E -> E] {
  irreflexive[(fre[x]) . (prop[x,ppo]) . *(hb[x,ppo])]
}


pred consistent[x : Exec_H] {     
  Uniproc[x]	
  some ppo : E -> E {
    valid_ppo[x,ppo]
    No_thin_air[x,ppo]
    Propagation[x,ppo]
    Causality[x,ppo]
  }
}
