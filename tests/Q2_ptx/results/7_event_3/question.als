open ../../hw/gpu/ptx[E] as M2
open ../../hw/gpu/ptx_cummulative[E] as M1

sig E {}

fun cta_fence_raw[x:Exec_PTX] : E->E {
  ((x.sb) . (stor[x.membar_cta]) . (x.sb))
}

fun gl_fence_raw[x:Exec_PTX] : E->E {
  ((x.sb) . (stor[x.membar_gl]) . (x.sb))
} 

fun sys_fence_raw[x:Exec_PTX] : E->E {
  (x.sb) . (stor[x.membar_sys]) . (x.sb)
} 


fun intra_cta_raw[x:Exec_PTX] : E->E {
  (x.scta - x.sthd) 
}

fun intra_gl_raw[x:Exec_PTX] : E->E {
  (x.sgl - x.scta) 
}



pred gp [X : Exec_PTX] {

  no X.(R&W)

  no ((X.ev -> X.ev) - X.sgl)

  no X.atom

  // The execution is forbidden in M1
  not(M1/consistent[X])
  M1/dead[X]

  // The execution is allowed (and not faulty) in M2
  M2/consistent[X]

  // Disallowing the first test
  irreflexive [
      (((X.R) -> (X.W)) & cta_fence_raw[X]) .
      ((X.rf) & intra_cta_raw[X]) .
      (((X.R) -> (X.W)) & sys_fence_raw[X]) .
      (fr[X] & intra_gl_raw[X]) . 
      ((X.rf) & intra_gl_raw[X])
      ]

  irreflexive [
      (((X.R) -> (X.R)) & cta_fence_raw[X]) .
      ((fr[X]) & intra_cta_raw[X]) .
      (((X.W) -> (X.W)) & gl_fence_raw[X]) .
      ((X.co) & intra_gl_raw[X]) . 
      ((X.rf) & intra_gl_raw[X])
      ]

  irreflexive [
      (((X.R) -> (X.W)) & sys_fence_raw[X]) .
      ((X.co) & intra_cta_raw[X]) .
      (((X.W) -> (X.W)) & cta_fence_raw[X]) .
      ((X.co) & intra_gl_raw[X]) . 
      ((X.rf) & intra_gl_raw[X])
      ]



}

run gp for exactly 1 Exec, 7 E 
