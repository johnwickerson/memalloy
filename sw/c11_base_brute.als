module c11_base_brute[E]
open exec_C_brute[E]

fun Fsb[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] : E -> E {
  (stor[F]) . sb
}

fun sbF[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] : E -> E {
  sb . (stor[F])
}

fun rs[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] : E -> E {
  let rs_prime = sthd + (ev -> (R & W)) |
  (co & rs_prime) - ((co - rs_prime) . co)
}

fun sw[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] : E -> E {
  ((stor[rel]) . (rc[Fsb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]]) . (stor[A & W]) . (rc[rs[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]]) .
       rf . (stor[R & A]) . (rc[sbF[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]]) . (stor[acq])) - sthd
}

fun hb[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] : E -> E {
  ^(sb + sw[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co])   
}

pred Hb[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  is_acyclic[hb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]]
}

pred Coh[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  irreflexive[(co + co.rf + (fr[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]) +
	       ((fr[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]) . rf)) . (hb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co])]
}

pred Rf[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  irreflexive[rf . (hb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co])]
}

fun hbl[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] : E -> E {
  (hb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]) & sloc
}

pred NaRf[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  let vis = ((stor[W]) . (hbl[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]) . (stor[R])) -
             ((hbl[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]) . (stor[W]) . (hbl[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co])) |
  (rf . (stor[naL])) in vis
}

pred Rmw[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  irreflexive[rf + (co . (fr[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co])) + (co.rf)]
}

fun cnf[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] : E -> E {
  ((W -> (R+W)) + ((R+W) -> W)) & sloc - iden
}

pred Dr[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  let dr = cnf[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co] - (hb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]) - ~(hb[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]) -
    (A -> A) - sthd |
  is_empty[dr]    
}

pred Ur[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  let ur = (cnf[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co] - sb - ~sb) & sthd |
  is_empty[ur]    
}

