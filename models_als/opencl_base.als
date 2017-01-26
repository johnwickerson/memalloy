/* Manually generated from opencl_base.cat */

module opencl_base[E]
open ../archs/exec_OpenCL[E]


fun Fsb[e:E, x : Exec_OpenCL] : E -> E {
  (stor[F[e,x]]) . (sb[e,x])
}

fun sbF[e:E, x : Exec_OpenCL] : E -> E {
  (sb[e,x]) . (stor[F[e,x]])
}

// inclusive scopes (conservative original)
fun incl'[e:E, x : Exec_OpenCL] : E -> E {
  swg[e,x] & (wg[e,x] -> wg[e,x]) +
  sdv[e,x] & (dv[e,x] -> dv[e,x]) +
             (sy[e,x] -> sy[e,x])
}

// inclusive scopes (less conservative version)
fun incl[e:E, x : Exec_OpenCL] : E -> E {
  let incl1 = (stor[wg[e,x]]) . (swg[e,x]) +
              (stor[dv[e,x]]) . (sdv[e,x]) +
              (stor[sy[e,x]]) . (ev[e,x] -> ev[e,x]) |
  (incl1 & ~incl1) + 
  ((stor[rem[e,x]]) . incl1) + 
  (~incl1 . (stor[rem[e,x]]))
}

fun rs[e:E, x : Exec_OpenCL] : E -> E {
  let rs_prime = sthd[e,x] + (ev[e,x] -> (R[e,x] & W[e,x])) |
  (co[e,x] & rs_prime) - ((co[e,x] - rs_prime) . (co[e,x]))     
}

fun scf[e:E, x : Exec_OpenCL] : E -> E {
  (sc[e,x] -> sc[e,x]) + ((G[e,x] & L[e,x] & F[e,x]) -> (G[e,x] & L[e,x] & F[e,x]))
}

fun swra_local[e:E, x : Exec_OpenCL] : E -> E {
  ((stor[L[e,x] & rel[e,x]]) . (rc[Fsb[e,x]]) . (stor[A[e,x] & W[e,x]]) .
  (rc[rs[e,x]]) . (stor[L[e,x]]) . (rf[e,x]) . (stor[R[e,x] & A[e,x]]) . (rc[sbF[e,x]]) .
  (stor[L[e,x] & acq[e,x]])) & (incl[e,x]) - (sthd[e,x])
}

fun swra_global[e:E, x : Exec_OpenCL] : E -> E {
  ((stor[G[e,x] & rel[e,x]]) . (rc[Fsb[e,x]]) . (stor[A[e,x] & W[e,x]]) .
  (rc[rs[e,x]]) . (stor[G[e,x]]) . (rf[e,x]) . (stor[R[e,x] & A[e,x]]) . (rc[sbF[e,x]]) .
  (stor[G[e,x] & acq[e,x]])) & (incl[e,x]) - (sthd[e,x])
}

fun bsw_local[e:E, x : Exec_OpenCL] : E -> E {
  (entry_fence[e,x] -> exit_fence[e,x]) & (sbar[e,x]) &
    (swg[e,x]) & (L[e,x] -> L[e,x]) - sthd[e,x]
}

fun bsw_global[e:E, x : Exec_OpenCL] : E -> E {
  (entry_fence[e,x] -> exit_fence[e,x]) & (sbar[e,x]) &
    (swg[e,x]) & (G[e,x] -> G[e,x]) - sthd[e,x]
}

fun sw_global[e:E, x : Exec_OpenCL] : E -> E {
  swra_global[e,x] + bsw_global[e,x] +
   (scf[e,x] & swra_local[e,x])
}

fun sw_local[e:E, x : Exec_OpenCL] : E -> E {
  swra_local[e,x] + bsw_local[e,x] +
   (scf[e,x] & swra_global[e,x])
}

fun ghb[e:E, x : Exec_OpenCL] : E -> E {
  ^(((G[e,x] -> G[e,x]) & sb[e,x]) + sw_global[e,x])
}

fun lhb[e:E, x : Exec_OpenCL] : E -> E {
  ^(((L[e,x] -> L[e,x]) & sb[e,x]) + sw_local[e,x])
}

fun hb[e:E, x : Exec_OpenCL] : E -> E {
  ghb[e,x] + lhb[e,x]
}

pred Ghb[e:E, x : Exec_OpenCL] {
  is_acyclic[ghb[e,x]]
}

pred Lhb[e:E, x : Exec_OpenCL] {
  is_acyclic[lhb[e,x]]
}

pred Gcoh[e:E, x : Exec_OpenCL] {
  irreflexive[(co[e,x] + (co[e,x]).(rf[e,x]) + (fr[e,x]) +
	       ((fr[e,x]) . (rf[e,x]))) . (ghb[e,x])]
}

pred Lcoh[e:E, x : Exec_OpenCL] {
  irreflexive[(co[e,x] + (co[e,x]).(rf[e,x]) + (fr[e,x]) +
	       ((fr[e,x]) . (rf[e,x]))) . (lhb[e,x])]
}

pred Rf[e:E, x : Exec_OpenCL] {
  irreflexive[(rf[e,x]) . (hb[e,x])]
}

pred GnaRf[e:E, x : Exec_OpenCL] {
  (rf[e,x] :> (naL[e,x] & G[e,x])) in imm[(stor[W[e,x]]) . (ghb[e,x] & sloc[e,x])]
}

pred LnaRf[e:E, x : Exec_OpenCL] {
  (rf[e,x] :> (naL[e,x] & L[e,x])) in imm[(stor[W[e,x]]) . (lhb[e,x] & sloc[e,x])]
}

pred Rmw[e:E, x : Exec_OpenCL] {
  irreflexive[rf[e,x] + ((co[e,x]) . (fr[e,x])) + ((co[e,x]) . (rf[e,x]))]
}

fun cnf[e:E, x : Exec_OpenCL] : E -> E {
  ((W[e,x] -> (R[e,x] + W[e,x])) + ((R[e,x] + W[e,x]) -> W[e,x])) & (sloc[e,x])
}

pred Dr[e:E, x : Exec_OpenCL] {
  let dr = cnf[e,x] - (hb[e,x]) - ~(hb[e,x]) - (sthd[e,x]) - incl[e,x] |
  is_empty[dr]
}

pred Bd[e:E, x : Exec_OpenCL] {
  let unv = ev[e,x] -> ev[e,x] |
  let bd = (stor[entry_fence[e,x]]) & ((~(sthd[e,x]) & swg[e,x]) . unv) -
    ((bsw_global[e,x] + bsw_local[e,x]) . unv) |   
  is_empty[bd]
}

pred racefree[e:E, x : Exec_OpenCL] {
  Dr[e,x]
  Bd[e,x]
}

pred dead[e:E, x : Exec_C] {

  // avoid "if(r==0)" in generated litmus test
  no_if_zero[e,x]

  // co edges can't be changed to make consistent exec
  forced_co[e,x]
 
  // potential data races are avoided by
  // separating them with dependable happens-before   
  let cde /* external control dependency */ =
    *((rf[e,x] - sthd[e,x]) + (cd[e,x])) . (cd[e,x]) |
  let not_cde = (ev[e,x] -> ev[e,x]) - cde |
  let drs /* dependable release sequence */ =
    (rs[e,x]) & (((sb[e,x] & sloc[e,x])) . *(rf[e,x])) - ((stor[R[e,x]]) . not_cde) |
  let dsw_global /* dependable synchronises-with */ =
    sw_global[e,x] & (((rc[Fsb[e,x]]) . (stor[rel[e,x]]) . 
      (rc[drs]) - (~(cd[e,x]) . not_cde)) . (rf[e,x])) |
  let dsw_local /* dependable synchronises-with */ =
    sw_local[e,x] & (((rc[Fsb[e,x]]) . (stor[rel[e,x]]) . 
      (rc[drs]) - (~(cd[e,x]) . not_cde)) . (rf[e,x])) |
  let dhb_global /* dependable happens-before */ =
    (rc[sb[e,x]]) . *(dsw_global . (cd[e,x])) |
  let dhb_local /* dependable happens-before */ =
    (rc[sb[e,x]]) . *(dsw_local . (cd[e,x])) |
  let pdr /* potential data race */ =
    cnf[e,x] - incl[e,x] |    
  pdr in dhb_global + ~dhb_global + dhb_local + ~dhb_local
    
}
