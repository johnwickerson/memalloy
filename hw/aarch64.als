/* Manually generated from aarch64.cat */

module aarch64[E]
open exec_arm8[E]

fun ctrlisb_[x:Exec_Arm8] : E->E {
  ^(x.cd) . (stor[x.isb]) . (x.sb)
}

fun dep[x:Exec_Arm8] : E->E {
  x.ad + x.dd
}

fun rdw[x:Exec_Arm8] : E->E {
  poloc[x] & (fre[x]) . (rfe[x])
}

fun detour[x:Exec_Arm8] : E->E {
  poloc[x] & (coe[x]) . (rfe[x])
} 

fun ppo[x:Exec_Arm8] : E->E {
  let ci0 = ctrlisb_[x] + detour[x] |
  let ii0 = dep[x] + rfi[x] + rdw[x] |
  let cc0 = dep[x] + x.cd + (x.ad) . (x.sb) |
  let ic0 = none -> none |
          
  let ci1 = ci0 + ci0.ii0 + cc0.ci0 |
  let ii1 = ii0 + ci0 + ic0.ci0 + ii0.ii0 |
  let cc1 = cc0 + ci0 + ci0.ic0 + cc0.cc0 |
  let ic1 = ic0 + ii0 + cc0 + ic0.cc0 + ii0.ic0 |
           
  let ci2 = ci0 + ci1.ii1 + cc1.ci1 |
  let ii2 = ii0 + ci1 + ic1.ci1 + ii1.ii1 |
  let cc2 = cc0 + ci1 + ci1.ic1 + cc1.cc1 |
  let ic2 = ic0 + ii1 + cc1 + ic1.cc1 + ii1.ic1 |
         
  let ci  = ci0 + ci2.ii2 + cc2.ci2 |
  let ii  = ii0 + ci2 + ic2.ci2 + ii2.ii2 |
  let cc  = cc0 + ci2 + ci2.ic2 + cc2.cc2 |
  let ic  = ic0 + ii2 + cc2 + ic2.cc2 + ii1.ic2 |

  let ppoR = ii & (x.R -> x.R) |
  let ppoW = ic & (x.R -> x.W) |
  ppoR + ppoW                          
}

fun acq_[x:Exec_Arm8] : E->E {
  (x.scacq -> x.ev) & x.sb
}

fun rel_[x:Exec_Arm8] : E->E {
  (x.ev -> x.screl) & x.sb
}

fun syf[x:Exec_Arm8] : E->E {
  (x.sb) . (stor[x.dmb]) . (x.sb)
}

fun stf[x:Exec_Arm8] : E->E {
  (stor[x.W]) . (x.sb) . (stor[x.dmbst]) . (x.sb) . (stor[x.W])
}

fun ldf[x:Exec_Arm8] : E->E {
  (stor[x.R]) . (x.sb) . (stor[x.dmbst]) . (x.sb)
}

fun fence[x:Exec_Arm8] : E->E {
  syf[x] + stf[x] + ldf[x] + acq_[x] + rel_[x]
}

fun hb[x:Exec_Arm8] : E->E {
  (stor[x.R]) . (fence[x]) + rfe[x] + ppo[x]
}

pred Thin_air[x:Exec_Arm8] {
  is_acyclic[hb[x]]
}

fun prop[x:Exec_Arm8] : E->E {
  *(com[x]) . (syf[x]) + stf[x] + (rc[rfe[x]]) . (rel_[x])
}

fun prop_al[x:Exec_Arm8] : E->E {
  ((x.screl -> x.scacq) & (x.rf + x.sb)) +
  ((x.scacq -> x.screl) & (fr[x]))
}

fun xx[x:Exec_Arm8] : E->E {
  (x.W -> x.W) & (x.A -> x.A) & x.sb
}

pred Observation[x:Exec_Arm8] {
  irreflexive[ (prop[x]) . (rfe[x]) . (fence[x] + ppo[x]) . (fre[x]) ]
}

pred Propagation[x:Exec_Arm8] {
  is_acyclic[
    x.co + (prop[x]) . *(hb[x]) +
    xx[x] . (prop_al[x]) . *(hb[x])
  ]
}

pred consistent[x:Exec_Arm8] {
  Uniproc[x]
  Atomic[x]
  Thin_air[x]
  Observation[x]
  Propagation[x]
}

// sanity check
run { some x:Exec_Arm8 | consistent[x] } for 1 Exec, 4 E
