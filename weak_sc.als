open archs/exec[E]

sig E {}

fun id [e:PTag->E, X:Exec] : E->E {
  stor[EV[e,X]]
}

fun uni [e:PTag->E, X:Exec] : E->E {
  (EV[e,X]) -> (EV[e,X])
}

fun M [e:PTag->E, X:Exec] : set E {
  (R[e,X]) + (W[e,X])
}

fun po [e:PTag->E, X:Exec] : E->E {
  sb[e,X]
}

fun poloc [e:PTag->E, X:Exec] : E->E {
  (po[e,X]) & (sloc[e,X])
}

fun addr [e:PTag->E, X:Exec] : E->E {
  ad[e,X]
}

fun ctrl [e:PTag->E, X:Exec] : E->E {
  cd[e,X]
}

fun data [e:PTag->E, X:Exec] : E->E {
  dd[e,X]
}

fun loc [e:PTag->E, X:Exec] : E->E {
  sloc[e,X]
}

fun thd [e:PTag->E, X:Exec] : E->E {
  sthd[e,X]
}

fun ext [e:PTag->E, X:Exec] : E->E {
  ((EV[e,X]) -> (EV[e,X])) - (sthd[e,X])
}

fun rfe [e:PTag->E, X:Exec] : E->E {
  (rf[e,X]) - (sthd[e,X])
}

fun rfi [e:PTag->E, X:Exec] : E->E {
  (rf[e,X]) & (sthd[e,X])
}

fun if_zero [e:PTag->E, X:Exec] : E->E {
  ((id[e,X]) & ((ctrl[e,X]) . (~(ctrl[e,X])))) - ((~(rf[e,X])) . (rf[e,X]))
}

fun imm [r:E->E,e:PTag->E, X:Exec] : E->E {
  (r) - ((r) . (^(r)))
}

/*
fun unforced_co [e:PTag->E, X:Exec] : E->E {
  ((imm[co[e,X],e,X]) . (imm[co[e,X],e,X]) . (~(imm[co[e,X],e,X]))) - ((rc[rf[e,X]]) . (rc[(po[e,X]) . (rc[~(rf[e,X])])]))
}
*/

/*
fun wco [e:PTag->E, X:Exec] : E->E {
  (((stor[W[e,X]]) . (rc[rf[e,X]]) . (^((po[e,X]) + (rf[e,X]))) . (rc[~(rf[e,X])]) . (stor[W[e,X]])) & (sloc[e,X])) - (id[e,X])
}
*/

fun fr_init [e:PTag->E, X:Exec] : E->E {
  ((stor[X.R]) - (~(X.rf) . (X.rf))) . (X.sloc) . (stor[X.W])
}

fun fr [e:PTag->E, X:Exec, co:E->E] : E->E {
  ((~(X.rf) . co) + fr_init[e,X]) - iden
}

fun wco_iter [e:PTag->E, X:Exec, wco:E->E] : E->E {
((stor[X.W]) . ^(X.sb + X.rf + fr_init[e,X] + wco + ~(X.rf).wco) . (rc[~(X.rf)]) . (stor[X.W])  - iden) & X.sloc
}

fun wco [e:PTag->E, X:Exec] : E->E {
let wco0 = none->none |
let wco1 = wco_iter[e,X, wco0] |
let wco2 = wco_iter[e,X, wco1] |
let wco3 = wco_iter[e,X, wco2] |
wco3
}



pred Seqcst [e:PTag->E, X:Exec, co:E->E] {
  is_acyclic[(po[e,X]) + (rf[e,X]) + (fr[e,X,co]) + co]
}

pred WeakSeqCst[e:PTag->E, X:Exec] {
  is_acyclic[(po[e,X]) + (rf[e,X]) + (fr[e,X,wco[e,X]]) + (wco[e,X])]
}

pred SimplePost [e:PTag->E, X:Exec] {
  is_empty[(wco[e,X]) - ((rc[rf[e,X]]) . (rc[(po[e,X]) . (rc[~(rf[e,X])])]))]
}

pred interesting[e:PTag->E, X:Exec] {

  not(some co : E->E | wf_co[X,co] and Seqcst[e,X,co])

  WeakSeqCst[e,X]
 
  SimplePost[e,X]

}

pred gp [X:Exec] {

  withoutinit[X]
  E in X.EV

  // Every event is a read, write, or fence
  E in X.R + X.W + X.F

  no (X.R & X.W)

  interesting[none->none, X]

  hint[X]
}

pred hint[X:Exec] {
// 0: W c 2  | 3: W c 1  | 6: W b 2  | 9:  W b 1
// 1: W a 2  | 4: R a 2  | 7: W a 1  | 10: R a 1
// 2: R b 2  | 5: R b 1  | 8: R c 2  | 11: R c 1
  	some disj e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11 : E {
		X.EV = e0+e1+e2+e3+e4+e5+e6+e7+e8+e9+e10+e11
		X.R = e2+e4+e5+e8+e10+e11
		X.W = e0+e1+e3+e6+e7+e9
		X.F = none
    	X.IW = none
		X.rf = (e6->e2) + (e1->e4) + (e9->e5)
		+ (e0->e8) + (e7->e10) + (e3->e11)
		X.sloc = sq[e1+e4+e7+e10] + sq[e2+e5+e6+e9] + sq[e0+e3+e8+e11]
		X.sthd = sq[e0+e1+e2] + sq[e3+e4+e5] + sq[e6+e7+e8] + sq[e9+e10+e11]
    	X.cd = none->none
    	X.dd = none->none
    	X.ad = none->none
    	X.sb = ^(e0->e1 + e1->e2) + ^(e3->e4 + e4->e5)
				+ ^(e6->e7 + e7->e8) + ^(e9->e10 + e10->e11)
  	}
}

pred hint_withinit[X:Exec] {
// 12: W a 0, 13: W b 0, 14: W c 0
//
// 0: W c 2  | 3: W c 1  | 6: W b 2  | 9:  W b 1
// 1: W a 2  | 4: R a 2  | 7: W a 1  | 10: R a 1
// 2: R b 2  | 5: R b 1  | 8: R c 2  | 11: R c 1
  	some disj e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14 : E {
		X.EV = e0+e1+e2+e3+e4+e5+e6+e7+e8+e9+e10+e11+e12+e13+e14
		X.R = e2+e4+e5+e8+e10+e11
		X.W = e0+e1+e3+e6+e7+e9+e12+e13+e14
		X.F = none
    	X.IW = e12+e13+e14
		X.rf = (e6->e2) + (e1->e4) + (e9->e5)
		+ (e0->e8) + (e7->e10) + (e3->e11)
		X.sloc = sq[e1+e4+e7+e10+e12] + sq[e2+e5+e6+e9+e13] + sq[e0+e3+e8+e11+e14]
		X.sthd = sq[e0+e1+e2] + sq[e3+e4+e5] + sq[e6+e7+e8] + sq[e9+e10+e11]
    	X.cd = none->none
    	X.dd = none->none
    	X.ad = none->none
    	X.sb = ^(e0->e1 + e1->e2) + ^(e3->e4 + e4->e5)
				+ ^(e6->e7 + e7->e8) + ^(e9->e10 + e10->e11)
  	}
}

run gp for 1 Exec, 5 E, 3 Int expect 0 // 9377 seconds (3 hrs)
run gp for 1 Exec, 6 E, 3 Int 
run gp for 1 Exec, 7 E, 3 Int expect 1
run gp for 1 Exec, 12 E, 3 Int expect 1 // 
