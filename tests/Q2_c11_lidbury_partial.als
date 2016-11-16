open ../sw/c11_lidbury[E] as M1
open ../sw/c11_partial[E] as M2

sig E {}

run {
  some X:Exec_C {
    not (M1/consistent[X])
    dead[X]
    M2/consistent[X]
 
    // just simplifying the solution:
    no_RMWs[X]
	no X.(ad+cd+dd)
    #(X.W) = #(X.R)
  }
} for 1 Exec, 4 E, 3 Int expect 1

// This example shows that if you run
//
// store(x,1,RLX); || r0 = load(y,SC);
// store(y,1,SC);  || r1 = load(x,SC);
//
// then in the "original" C11 model you are
// allowed to observe r0==1&&r1==0, but in the
// Lidbury-Donaldson simplified model, you 
// cannot observe that outcome.
