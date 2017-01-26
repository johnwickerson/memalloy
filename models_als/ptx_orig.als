module ptx_orig[E]
open ../archs/exec_ptx[E]
open basic[E]

pred Atomic[e:E, x:Exec_PTX] {
  is_empty[atom[e,x] & ((fre[e,x]) . (coe[e,x]))]
}

pred ScPerLocLLH[e:E, x:Exec_PTX] {
  let com = rf[e,x] + fr[e,x] + co[e,x] |
  let polocLLH = poloc[e,x] & (((R[e,x] + W[e,x]) -> W[e,x]) + (W[e,x] -> (R[e,x] + W[e,x]))) |
  is_acyclic[polocLLH + com]
}

fun dp[e:E, x:Exec_PTX] : E->E{
  ad[e,x] + dd[e,x] + cd[e,x]
}

pred NoThinAir[e:E, x:Exec_PTX] {
  is_acyclic[dp[e,x] + rf[e,x]]
}

fun sys_fence[e:E, x:Exec_PTX] : E->E {
  (sb[e,x]) . (stor[membar_sys[e,x]]) . (sb[e,x])
} 

fun gl_fence[e:E, x:Exec_PTX] : E->E {
  ((sb[e,x]) . (stor[membar_gl[e,x]]) . (sb[e,x])) + sys_fence[e,x]
} 

fun cta_fence[e:E, x:Exec_PTX] : E->E {
  ((sb[e,x]) . (stor[membar_cta[e,x]]) . (sb[e,x])) + gl_fence[e,x]
} 

fun rmo[e:E, x:Exec_PTX, f:E->E] : E->E {
  dp[e,x] + rfe[e,x] + co[e,x] + fr[e,x] + f
}

pred CTAconstraint[e:E, x:Exec_PTX] {
  is_acyclic[rmo[e,x,cta_fence[e,x]] & scta[e,x]]
}

pred GLconstraint[e:E, x:Exec_PTX] {
  is_acyclic[rmo[e,x,gl_fence[e,x]] & sgl[e,x]]
}

pred SYSconstraint[e:E, x:Exec_PTX] {
  is_acyclic[rmo[e,x,sys_fence[e,x]]]
}

pred consistent[e:E, x:Exec_PTX] {
  Atomic[e,x]
  ScPerLocLLH[e,x]
  NoThinAir[e,x]
  CTAconstraint[e,x]
  GLconstraint[e,x]
  SYSconstraint[e,x]
}
