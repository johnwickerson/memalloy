/* Manually generated from c11_base.cat */

module c11_base[E]
open exec_C[E]

fun Fsb[x : Exec_C] : E -> E {
  (stor[x.F]) . (x.sb)
}

fun sbF[x : Exec_C] : E -> E {
  (x.sb) . (stor[x.F])
}

fun rs[x : Exec_C] : E -> E {
  *(x.sb & x.sloc) . *(x.rf)
}

fun sw[x : Exec_C] : E -> E {
  ((stor[x.rel]) . (rc[Fsb[x]]) . (stor[x.(A & W)]) . (rs[x]) .
       (x.rf) . (stor[x.(R & A)]) . (rc[sbF[x]]) . (stor[x.acq])) - (x.sthd)
}

fun hb[x : Exec_C] : E -> E {
  ^(x.sb + sw[x])   
}

fun hbl[x : Exec_C] : E -> E {
  (hb[x]) & (x.sloc)
}

pred NaRf[x : Exec_C] {
  ((x.rf) . (stor[x.naL])) in imm[(stor[x.W]) . (hbl[x])]
}

fun cnf[x : Exec_C] : E -> E {
  ((x.W -> x.W) + (x.W -> x.R) + (x.R -> x.W)) & (x.sloc) - iden
}

pred Dr[x : Exec_C] { 
  cnf[x] - (x.A -> x.A) - (x.sthd) in (hb[x]) + ~(hb[x]) 
}

pred Ur[x : Exec_C] {
  cnf[x] & (x.sthd) in (x.sb) + ~(x.sb)    
}

pred HbCom[x:Exec_C] {
  is_acyclic[ ((hb[x]) & x.sloc) + x.rf + x.co + (fr[x]) ]
}

pred Ssimp[x : Exec_C] {
  let scb = x.co + fr[x] + (hb[x]) |
  let FscbF = (rc[Fsb[x]]) . scb . (rc[sbF[x]]) |
  let scp = FscbF & (x.sc -> x.sc) - iden |
  is_acyclic[scp]
}

pred racefree[x : Exec_C] {
  Dr[x]
  Ur[x]
}

pred dead_base[x:Exec_C] {

  // avoid "if(r==0)" in generated litmus test
  no_if_zero[x]

  // no unsequenced races
  Ur[x]
 
  // potential data races are avoided, either by
  // separating them with dependable happens-before, or
  // by having the read in a self-satisfying cycle    
  let cde /* external control dependency */ =
    *((x.rf - x.sthd) + (x.cd)) . (x.cd) |
  let not_cde = (x.ev -> x.ev) - cde |
  let drs /* dependable release sequence */ =
    (rs[x]) - ((stor[x.R]) . not_cde) |
  let dsw /* dependable synchronises-with */ =
    sw[x] & (((rc[Fsb[x]]) . (stor[x.rel]) . 
      (rc[drs]) - (~(x.cd) . not_cde)) . (x.rf)) |
  let dhb /* dependable happens-before */ =
    (rc[x.sb]) . *(dsw . (x.cd)) |
  let ssc /* self-satisfying cycle */ =
    iden & cde |
  let pdr /* potential data race */ =
    cnf[x] - (x.A -> x.A) |
  let narf /* reads-from on non-atomic location */ =
    x.rf & (x.naL -> x.naL) |      
  pdr in dhb + ~dhb + narf.ssc + ssc.~narf

}

pred dead[x : Exec_C] {

  dead_base[x]

  // co edges can't be changed to make consistent exec
  forced_co[x]
    
}
