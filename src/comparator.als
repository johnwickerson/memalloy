/* Searching for an execution allowed by Batty et al's C11 model but disallowed by Lidbury et al.'s model. */
open ../models/c11_partial[E] as M1
open ../models/c11_normws[E] as M2
open ../models/c11_lidbury[E] as N1
sig E {}

pred gp [X:Exec_C] {

  // Every event is a read, write or a fence
  E in R[none,X] + W[none,X] + F[none,X]

  withoutinit[X]

  not(N1/consistent[none,X])
  N1/dead[none,X]

  M1/consistent[none,X]
  M2/consistent[none,X]
  // Total sb per thread
  total_sb[none,X]
}

run gp for 1 Exec, 4 E, 3 Int
