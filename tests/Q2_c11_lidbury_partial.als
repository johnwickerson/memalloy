open ../models_als/c11_lidbury[E] as M1
open ../models_als/c11_partial[E] as M2

sig E {}

pred distinguishes[X:Exec_C] {
  not (M1/consistent[none,X])
  M1/dead[none,X]
  M2/consistent[none,X]

  // For now, omit dependencies
  no ad[none,X] + cd[none,X] + dd[none,X]

  // For now, omit RMWs
  no R[none,X] & W[none,X]
}

run t1 { some X:Exec_C {
  distinguishes[X]
}} for 1 Exec, 4 E, 3 Int expect 1
// Gives:
// store(x,2,SC); || r0 = load(y,RLX);
// store(y,1,SC); || store(x,1,SC);
// { r0==1 && x==2 }

fun cycle1[X:Exec_C] : E->E {
  (stor[X.W & X.sc]) .
  (X.sb - X.sloc) .
  (stor[X.W & X.sc]) .
  (X.rf) .
  (stor[X.R & X.(A - acq)]) .
  (X.sb - X.sloc) .
  (stor[X.W & X.sc]) .
  (X.co)
}

run t2 { some X:Exec_C {
  distinguishes[X]
  irreflexive[cycle1[X]]
}} for 1 Exec, 4 E, 3 Int expect 1
// Gives:
// store(x,2,SC); || store(y,2,RLX);
// store(y,1,SC); || store(x,1,RLX);
// { x==2 && y==2 }


fun cycle2[X:Exec_C] : E->E {
  (stor[X.W & X.sc]) .
  (X.sb - X.sloc) .
  (stor[X.W & X.sc]) .
  (X.co) .
  (stor[X.W & X.(A - rel)]) .
  (X.sb - X.sloc) .
  (stor[X.W & X.(A - rel)]) .
  (X.co)
}

run t3 { some X:Exec_C {
  distinguishes[X]
  irreflexive[cycle1[X]]
  irreflexive[cycle2[X]]
}} for 1 Exec, 4 E, 3 Int expect 1
// Gives:
// store(x,2,SC);  || r0 = load(y,SC);
// store(y,1,RLX); || store(x,1,REL);
// { x==2 && r0==1 }

fun cycle3[X:Exec_C] : E->E {
  (stor[X.W & X.sc]) .
  (X.sb - X.sloc) .
  (stor[X.W & X.(A - rel)]) .
  (X.rf) .
  (stor[X.R & X.sc]) .
  (X.sb - X.sloc) .
  (stor[X.W & X.(rel - sc)]) .
  (X.co)
}

run t4 { some X:Exec_C {
  distinguishes[X]
  irreflexive[cycle1[X]]
  irreflexive[cycle2[X]]
  irreflexive[cycle3[X]]
}} for 1 Exec, 4 E, 3 Int expect 1
// Gives:
// store(x,1,SC); || store(y,2,RLX);
// store(y,1,SC); || r0 = load(x,SC);
// { y==2 && r0==0 }

fun cycle4[X:Exec_C] : E->E {
  (stor[X.W & X.sc]) .
  (X.sb - X.sloc) .
  (stor[X.W & X.sc]) .
  (X.co) .
  (stor[X.W & X.(A - rel)]) .
  (X.sb - X.sloc) .
  (stor[X.R & X.sc]) .
  (fr[none,X])
}

run t5 { some X:Exec_C {
  distinguishes[X]
  irreflexive[cycle1[X]]
  irreflexive[cycle2[X]]
  irreflexive[cycle3[X]]
  irreflexive[cycle4[X]]
}} for 1 Exec, 4 E, 3 Int expect 1
// Gives: ...........




////////////////////////////////////

run t0 { some X:Exec_C {
  distinguishes[X]
  some disj e0,e1,e2:E {
    e0 in X.(W-sc)
    e1 in X.(W&sc)
    e2 in X.(R-sc)
    (e0->e2) in X.rf
    (e0->e1) in M1/hb[none,X] 
    (e0->e1) in X.sloc
  }
}} for 1 Exec, 5 E, 3 Int
