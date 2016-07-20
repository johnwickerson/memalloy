module ptx[E]
open exec_ptx[E]

// Not needed as Atomic is defined in exec_H
//pred Atomic[x:Exec_PTX] {
//  is_empty[x.atom & ((fre[x]) . (coe[x]))]
//}

pred ScPerLocLLH[x:Exec_PTX] {
  let com = x.rf + fr[x] + x.co |
  let polocLLH = poloc[x] & ((x.(R+W) -> x.W) + (x.W -> x.(R+W))) |
  is_acyclic[polocLLH + com]
}

fun dp[x:Exec_PTX] : E->E{
  x.ad + x.dd + x.cd
}

pred NoThinAir[x:Exec_PTX] {
  is_acyclic[dp[x] + x.rf]
}

fun sys_fence[x:Exec_PTX] : E->E {
  (x.sb) . (stor[x.membar_sys]) . (x.sb)
} 

fun gl_fence[x:Exec_PTX] : E->E {
  ((x.sb) . (stor[x.membar_gl]) . (x.sb)) + sys_fence[x]
} 

fun cta_fence[x:Exec_PTX] : E->E {
  ((x.sb) . (stor[x.membar_cta]) . (x.sb)) + gl_fence[x]
} 

fun rmo[x:Exec_PTX, f:E->E] : E->E {
  dp[x] + rfe[x] + x.co + fr[x] + f
}

pred CTAconstraint[x:Exec_PTX] {
  is_acyclic[rmo[x,cta_fence[x]] & x.scta]
}

pred GLconstraint[x:Exec_PTX] {
  is_acyclic[rmo[x,gl_fence[x]] & x.sgl]
}

pred SYSconstraint[x:Exec_PTX] {
  is_acyclic[rmo[x,sys_fence[x]]]
}

pred consistent[x:Exec_PTX] {
  Atomic[x]
  ScPerLocLLH[x]
  NoThinAir[x]
  CTAconstraint[x]
  GLconstraint[x]
  SYSconstraint[x]
}
