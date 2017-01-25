/* Manually generated from aarch64.cat */

module aarch64[E]
open exec_arm8[E]

fun ctrlisb_[e:E, x:Exec_Arm8] : E->E {
  ^(cd[e,x]) . (stor[isb[e,x]]) . (sb[e,x])
}

fun dep[e:E, x:Exec_Arm8] : E->E {
  ad[e,x] + dd[e,x]
}

fun rdw[e:E, x:Exec_Arm8] : E->E {
  poloc[e,x] & (fre[e,x]) . (rfe[e,x])
}

fun detour[e:E, x:Exec_Arm8] : E->E {
  poloc[e,x] & (coe[e,x]) . (rfe[e,x])
} 

fun ppo[e:E, x:Exec_Arm8] : E->E {
  let ci0 = ctrlisb_[e,x] + detour[e,x] |
  let ii0 = dep[e,x] + rfi[e,x] + rdw[e,x] |
  let cc0 = dep[e,x] + cd[e,x] + (ad[e,x]) . (sb[e,x]) |
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

  let ppoR = ii & (R[e,x] -> R[e,x]) |
  let ppoW = ic & (R[e,x] -> W[e,x]) |
  ppoR + ppoW                          
}

fun acq_[e:E, x:Exec_Arm8] : E->E {
  (scacq[e,x] -> ev[e,x]) & sb[e,x]
}

fun rel_[e:E, x:Exec_Arm8] : E->E {
  (ev[e,x] -> screl[e,x]) & sb[e,x]
}

fun syf[e:E, x:Exec_Arm8] : E->E {
  (sb[e,x]) . (stor[dmb[e,x]]) . (sb[e,x])
}

fun stf[e:E, x:Exec_Arm8] : E->E {
  (stor[W[e,x]]) . (sb[e,x]) . (stor[dmbst[e,x]]) . (sb[e,x]) . (stor[W[e,x]])
}

fun ldf[e:E, x:Exec_Arm8] : E->E {
  (stor[R[e,x]]) . (sb[e,x]) . (stor[dmbst[e,x]]) . (sb[e,x])
}

fun fence[e:E, x:Exec_Arm8] : E->E {
  syf[e,x] + stf[e,x] + ldf[e,x] + acq_[e,x] + rel_[e,x]
}

fun hb[e:E, x:Exec_Arm8] : E->E {
  (stor[R[e,x]]) . (fence[e,x]) + rfe[e,x] + ppo[e,x]
}

pred Thin_air[e:E, x:Exec_Arm8] {
  is_acyclic[hb[e,x]]
}

fun prop[e:E, x:Exec_Arm8] : E->E {
  *(com[e,x]) . (syf[e,x]) + stf[e,x] + (rc[rfe[e,x]]) . (rel_[e,x])
}

fun prop_al[e:E, x:Exec_Arm8] : E->E {
  ((screl[e,x] -> scacq[e,x]) & (rf[e,x] + sb[e,x])) +
  ((scacq[e,x] -> screl[e,x]) & (fr[e,x]))
}

fun xx[e:E, x:Exec_Arm8] : E->E {
  (W[e,x] -> W[e,x]) & (A[e,x] -> A[e,x]) & sb[e,x]
}

pred Observation[e:E, x:Exec_Arm8] {
  irreflexive[ (prop[e,x]) . (rfe[e,x]) . (fence[e,x] + ppo[e,x]) . (fre[e,x]) ]
}

pred Propagation[e:E, x:Exec_Arm8] {
  is_acyclic[
    co[e,x] + (prop[e,x]) . *(hb[e,x]) +
    xx[e,x] . (prop_al[e,x]) . *(hb[e,x])
  ]
}

pred consistent[e:E, x:Exec_Arm8] {
  Uniproc[e,x]
  Atomic[e,x]
  Thin_air[e,x]
  Observation[e,x]
  Propagation[e,x]
}

// sanity check
run { some x:Exec_Arm8 | consistent[none,x] } for 1 Exec, 4 E
