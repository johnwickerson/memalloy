/* Manually generated from c11_base.cat */

module c11_base[E]
open ../archs/exec_C[E]

fun Fsb[e:E, x : Exec_C] : E -> E {
  (stor[F[e,x]]) . (sb[e,x])
}

fun sbF[e:E, x : Exec_C] : E -> E {
  (sb[e,x]) . (stor[F[e,x]])
}

fun rs[e:E, x : Exec_C] : E -> E {
  *(sb[e,x] & sloc[e,x]) . *(rf[e,x])
}

fun sw[e:E, x : Exec_C] : E -> E {
  ((stor[rel[e,x]]) . (rc[Fsb[e,x]]) . (stor[A[e,x] & W[e,x]]) . (rs[e,x]) .
       (rf[e,x]) . (stor[R[e,x] & A[e,x]]) . (rc[sbF[e,x]]) . (stor[acq[e,x]])) - (sthd[e,x])
}

fun hb[e:E, x : Exec_C] : E -> E {
  ^(sb[e,x] + sw[e,x])   
}

fun hbl[e:E, x : Exec_C] : E -> E {
  (hb[e,x]) & (sloc[e,x])
}

pred NaRf[e:E, x : Exec_C] {
  ((rf[e,x]) . (stor[naL[e,x]])) in imm[(stor[W[e,x]]) . (hbl[e,x])]
}

fun cnf[e:E, x : Exec_C] : E -> E {
  ((W[e,x] -> W[e,x]) + (W[e,x] -> R[e,x]) + (R[e,x] -> W[e,x])) & (sloc[e,x]) - iden
}

pred Dr[e:E, x : Exec_C] { 
  cnf[e,x] - (A[e,x] -> A[e,x]) - (sthd[e,x]) in (hb[e,x]) + ~(hb[e,x]) 
}

pred Ur[e:E, x : Exec_C] {
  cnf[e,x] & (sthd[e,x]) in (sb[e,x]) + ~(sb[e,x])    
}

pred HbCom[e:E, x:Exec_C] {
  is_acyclic[ ((hb[e,x]) & sloc[e,x]) + rf[e,x] + co[e,x] + (fr[e,x]) ]
}

pred Ssimp[e:E, x : Exec_C] {
  let scb = co[e,x] + fr[e,x] + (hb[e,x]) |
  let FscbF = (rc[Fsb[e,x]]) . scb . (rc[sbF[e,x]]) |
  let scp = FscbF & (sc[e,x] -> sc[e,x]) - iden |
  is_acyclic[scp]
}

pred racefree[e:E, x : Exec_C] {
  Dr[e,x]
  Ur[e,x]
}

pred dead_base[e:E, x:Exec_C] {

  // avoid "if(r==0)" in generated litmus test
  no_if_zero[e,x]

  // no unsequenced races
  Ur[e,x]
 
  // potential data races are avoided, either by
  // separating them with dependable happens-before, or
  // by having the read in a self-satisfying cycle    
  let cde /* external control dependency */ =
    *((rf[e,x] - sthd[e,x]) + (cd[e,x])) . (cd[e,x]) |
  let not_cde = (ev[e,x] -> ev[e,x]) - cde |
  let drs /* dependable release sequence */ =
    (rs[e,x]) - ((stor[R[e,x]]) . not_cde) |
  let dsw /* dependable synchronises-with */ =
    sw[e,x] & (((rc[Fsb[e,x]]) . (stor[rel[e,x]]) . 
      (rc[drs]) - (~(cd[e,x]) . not_cde)) . (rf[e,x])) |
  let dhb /* dependable happens-before */ =
    (rc[sb[e,x]]) . *(dsw . (cd[e,x])) |
  let ssc /* self-satisfying cycle */ =
    iden & cde |
  let pdr /* potential data race */ =
    cnf[e,x] - (A[e,x] -> A[e,x]) |
  let narf /* reads-from on non-atomic location */ =
    rf[e,x] & (naL[e,x] -> naL[e,x]) |      
  pdr in dhb + ~dhb + narf.ssc + ssc.~narf

}

pred dead[e:E, x : Exec_C] {

  dead_base[e,x]

  // co edges can't be changed to make consistent exec
  forced_co[e,x]
    
}
