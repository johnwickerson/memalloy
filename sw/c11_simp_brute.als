
module c11_simp_brute[E]
open c11_base_brute[E]

pred Ssimp[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  let scp = ((rc[Fsb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]]) . (co + fr[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co] + (hb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co])) .
    (rc[sbF[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]])) & (sc -> sc) - iden |
  is_acyclic[scp]
}

pred consistent[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  Hb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  Coh[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  Rf[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  NaRf[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  Rmw[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  Ssimp[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
}		      		      

pred racefree[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  Dr[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  Ur[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
}

pred dead[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  co in (rc[rf]) . sb . (rc[~rf])
}

pred safe[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {

  // C1
  Ur[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  
  // C2
  dom[cd] in ran[rf]
 
  // C3
  co in (rc[rf]) . sb . (rc[~rf])
 
  // C4
  let cde /* external control dependency */ =
    *((rf - sthd) + cd) . cd |
  let not_cde = (ev -> ev) - cde |
  let drs /* dependable release sequence */ =
    (stor[rel]) .
      (((sb & sloc) . *rf) - ((stor[R]) . not_cde)) |
  let dsw /* dependable synchronises-with */ =
    sw[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co] & (((rc[Fsb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]]) . (rc[drs]) - (~cd . not_cde)) . rf) |
  let dhb /* dependable happens-before */ =
    (rc[sb]) . *(dsw . cd) |
  let ssc /* self-satisfying cycle */ =
    iden & cde |
  let pdr /* potential data race */ =
    cnf[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co] - (A -> A) |
  let narf /* reads-from on non-atomic location */ =
    rf & (naL -> naL) |      
  pdr in dhb + ~dhb + narf.ssc + ssc.~narf

}
