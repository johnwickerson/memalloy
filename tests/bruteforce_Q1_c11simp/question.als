open ../../sw/c11_simp_brute[E] as M1
open ../../sw/preexecs_C_brute[E] as tl

sig E {}

// This is broken because of the "all x : Exec" for |Exec|=finite.
// Need to get rid of the Exec signature for this test.
/*
pred find_test_broken [p : Prog, post : Reg -> Val] {
 
  // Prefer solutions without RMWs
  //no Cas

  some x : Exec | cand_exec'[p,x,post]

  all x : Exec, post' : Reg -> Val | (
    cand_exec'[p,x,post'] &&
    M1/consistent[x]
  ) implies (
    M1/racefree[x] && post != post'
  )
}
*/


pred find_test [p : Prog, post : Addr -> Val] {

  some ev, W, R, F, A, naL, acq, rel, sc : set E |
  some sb, ad, dd, cd, sthd, sloc, rf, co : E->E |
    wf[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co] &&
    cand_exec'[p,ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co,post]

  all ev, W, R, F, A, naL, acq, rel, sc : set E |
  all sb, ad, dd, cd, sthd, sloc, rf, co : E->E |
  all post' : Addr -> Val | (
    wf[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co] &&
    cand_exec'[p,ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co,post'] &&
    M1/consistent[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  ) implies (
    post != post' &&
    M1/racefree[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
  )

  some Inst

}

pred hint[p:Prog, post:Addr->Val] {
  /*
  store(x,1); //i1
  r=load(x); //i2
  */
  some v1 : Val, x : ALoc, r : Reg, c : Seq, i1 : Store, i2 : Load {
    Loc = x
    Reg = r
    p.top = c
    p.cm = c+i1+i2
    p.fst = (c -> i1)
    p.snd = (c -> i2)
    i1.wval = v1
    i1.loc = x
    i2.loc = x
    i2.reg = r
    v1 != Zero
    post = (r -> Zero) + (x -> v1)
  }
}

pred p1[p:Prog, post:Addr->Val] {
  find_test[p,post]
  hint[p,post]
}

pred p2[p:Prog, post:Addr->Val] {
  find_test[p,post]
}

run p1 for 4 Int, 2 E, 1 Prog, 2 Val, 1 Loc, 1 Reg, 3 Cmd expect 1
// Takes <1s with glucose on babillion.

run p2 for 4 Int, 2 E, 1 Prog, 2 Val, 1 Loc, 1 Reg, 3 Cmd
// Runs out of memory

