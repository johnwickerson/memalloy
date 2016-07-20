module ptx_cummulative[E]
open exec_ptx[E]

// a draft of the cummulative-across-scopes PTX model

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

fun rmo[x:Exec_PTX, r:E->E, f:E->E] : E->E {
  dp[x] + rfe[x] + x.co + fr[x] + ((rc[r]) . f . (rc[r]))
}

pred consistent[x:Exec_PTX] {
  Atomic[x]
  ScPerLocLLH[x]
  NoThinAir[x]
  let CTArmo = rmo[x, iden, cta_fence[x]] & x.scta |
  let GLrmo = rmo[x, ^(CTArmo), gl_fence[x]] & x.sgl |
  let SYSrmo = rmo[x, ^(GLrmo), sys_fence[x]] |
  is_acyclic[CTArmo] && is_acyclic[GLrmo] && is_acyclic[SYSrmo]
}
