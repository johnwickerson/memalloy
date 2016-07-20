/* Manually generated from opencl_base.cat */

module opencl_base[E]
open exec_OpenCL[E]


fun Fsb[x : Exec_OpenCL] : E -> E {
  (stor[x.F]) . (x.sb)
}

fun sbF[x : Exec_OpenCL] : E -> E {
  (x.sb) . (stor[x.F])
}

// inclusive scopes (conservative original)
fun incl'[x : Exec_OpenCL] : E -> E {
  x.swg & (x.wg -> x.wg) +
  x.sdv & (x.dv -> x.dv) +
          (x.sy -> x.sy)
}

// inclusive scopes (less conservative version)
fun incl[x : Exec_OpenCL] : E -> E {
  let incl1 = (stor[x.wg]) . (x.swg) +
              (stor[x.dv]) . (x.sdv) +
              (stor[x.sy]) . (x.ev -> x.ev) |
  (incl1 & ~incl1) + 
  ((stor[x.rem]) . incl1) + 
  (~incl1 . (stor[x.rem]))
}

fun rs[x : Exec_OpenCL] : E -> E {
  let rs_prime = x.sthd + (x.ev -> (x.(R & W))) |
  (x.co & rs_prime) - ((x.co - rs_prime) . (x.co))     
}

fun scf[x : Exec_OpenCL] : E -> E {
  (x.sc -> x.sc) + (x.(G & L & F) -> x.(G & L & F))
}

fun swra_local[x : Exec_OpenCL] : E -> E {
  ((stor[x.(L & rel)]) . (rc[Fsb[x]]) . (stor[x.(A & W)]) .
  (rc[rs[x]]) . (stor[x.L]) . (x.rf) . (stor[x.(R & A)]) . (rc[sbF[x]]) .
  (stor[x.(L & acq)])) & (incl[x]) - (x.sthd)
}

fun swra_global[x : Exec_OpenCL] : E -> E {
  ((stor[x.(G & rel)]) . (rc[Fsb[x]]) . (stor[x.(A & W)]) .
  (rc[rs[x]]) . (stor[x.G]) . (x.rf) . (stor[x.(R & A)]) . (rc[sbF[x]]) .
  (stor[x.(G & acq)])) & (incl[x]) - (x.sthd)
}

fun bsw_local[x : Exec_OpenCL] : E -> E {
  (x.entry_fence -> x.exit_fence) & (x.sbar) &
    (x.swg) & (x.L -> x.L) - x.sthd
}

fun bsw_global[x : Exec_OpenCL] : E -> E {
  (x.entry_fence -> x.exit_fence) & (x.sbar) &
    (x.swg) & (x.G -> x.G) - x.sthd
}

fun sw_global[x : Exec_OpenCL] : E -> E {
  swra_global[x] + bsw_global[x] +
   (scf[x] & swra_local[x])
}

fun sw_local[x : Exec_OpenCL] : E -> E {
  swra_local[x] + bsw_local[x] +
   (scf[x] & swra_global[x])
}

fun ghb[x : Exec_OpenCL] : E -> E {
  ^(((x.G -> x.G) & x.sb) + sw_global[x])
}

fun lhb[x : Exec_OpenCL] : E -> E {
  ^(((x.L -> x.L) & x.sb) + sw_local[x])
}

fun hb[x : Exec_OpenCL] : E -> E {
  ghb[x] + lhb[x]
}

pred Ghb[x : Exec_OpenCL] {
  is_acyclic[ghb[x]]
}

pred Lhb[x : Exec_OpenCL] {
  is_acyclic[lhb[x]]
}

pred Gcoh[x : Exec_OpenCL] {
  irreflexive[(x.co + (x.co).(x.rf) + (fr[x]) +
	       ((fr[x]) . (x.rf))) . (ghb[x])]
}

pred Lcoh[x : Exec_OpenCL] {
  irreflexive[(x.co + (x.co).(x.rf) + (fr[x]) +
	       ((fr[x]) . (x.rf))) . (lhb[x])]
}

pred Rf[x : Exec_OpenCL] {
  irreflexive[(x.rf) . (hb[x])]
}

pred GnaRf[x : Exec_OpenCL] {
  (x.rf :> (x.naL & x.G)) in imm[(stor[x.W]) . (ghb[x] & x.sloc)]
}

pred LnaRf[x : Exec_OpenCL] {
  (x.rf :> (x.naL & x.L)) in imm[(stor[x.W]) . (lhb[x] & x.sloc)]
}

pred Rmw[x : Exec_OpenCL] {
  irreflexive[x.rf + ((x.co) . (fr[x])) + ((x.co) . (x.rf))]
}

fun cnf[x : Exec_OpenCL] : E -> E {
  ((x.W -> x.(R+W)) + (x.(R+W) -> x.W)) & (x.sloc)
}

pred Dr[x : Exec_OpenCL] {
  let dr = cnf[x] - (hb[x]) - ~(hb[x]) - (x.sthd) - incl[x] |
  is_empty[dr]
}

pred Bd[x : Exec_OpenCL] {
  let unv = x.ev -> x.ev |
  let bd = (stor[x.entry_fence]) & ((~(x.sthd) & x.swg) . unv) -
    ((bsw_global[x] + bsw_local[x]) . unv) |   
  is_empty[bd]
}

pred dead[x : Exec_C] {

  // avoid "if(r==0)" in generated litmus test
  no_if_zero[x]

  // co edges can't be changed to make consistent exec
  forced_co[x]
 
  // potential data races are avoided by
  // separating them with dependable happens-before   
  let cde /* external control dependency */ =
    *((x.rf - x.sthd) + (x.cd)) . (x.cd) |
  let not_cde = (x.ev -> x.ev) - cde |
  let drs /* dependable release sequence */ =
    (rs[x]) & ((x.(sb & sloc)) . *(x.rf)) - ((stor[x.R]) . not_cde) |
  let dsw_global /* dependable synchronises-with */ =
    sw_global[x] & (((rc[Fsb[x]]) . (stor[x.rel]) . 
      (rc[drs]) - (~(x.cd) . not_cde)) . (x.rf)) |
  let dsw_local /* dependable synchronises-with */ =
    sw_local[x] & (((rc[Fsb[x]]) . (stor[x.rel]) . 
      (rc[drs]) - (~(x.cd) . not_cde)) . (x.rf)) |
  let dhb_global /* dependable happens-before */ =
    (rc[x.sb]) . *(dsw_global . (x.cd)) |
  let dhb_local /* dependable happens-before */ =
    (rc[x.sb]) . *(dsw_local . (x.cd)) |
  let pdr /* potential data race */ =
    cnf[x] - incl[x] |    
  pdr in dhb_global + ~dhb_global + dhb_local + ~dhb_local
    
}

fact { all x : Exec_OpenCL | withoutinit[x] }
