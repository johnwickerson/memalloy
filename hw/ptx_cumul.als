module ptx_cumul[E]
open exec_ptx[E]

// a draft of the cummulative-across-scopes PTX model

pred ScPerLocLLH[e:E, x:Exec_PTX] {
  let com = rf[e,x] + fr[e,x] + co[e,x] |
  let polocLLH = poloc[e,x] & (((R[e,x] + W[e,x]) -> W[e,x]) + (W[e,x] -> (R[e,x] + W[e,x]))) |
  is_acyclic[polocLLH + com]
}

fun dp[e:E, x:Exec_PTX] : E->E {
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

fun rmo[e:E, x:Exec_PTX, r:E->E, f:E->E] : E->E {
  dp[e,x] + rfe[e,x] + co[e,x] + fr[e,x] + ((rc[r]) . f . (rc[r]))
}

pred consistent[e:E, x:Exec_PTX] {
  Atomic[e,x]
  ScPerLocLLH[e,x]
  NoThinAir[e,x]
  let CTArmo = rmo[e, x, iden, cta_fence[e,x]] & scta[e,x] |
  let GLrmo = rmo[e, x, ^(CTArmo), gl_fence[e,x]] & sgl[e,x] |
  let SYSrmo = rmo[e, x, ^(GLrmo), sys_fence[e,x]] |
  is_acyclic[CTArmo] && is_acyclic[GLrmo] && is_acyclic[SYSrmo]
}
