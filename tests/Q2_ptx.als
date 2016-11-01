open ../hw/ptx_orig[E] as M2
open ../hw/ptx_cumul[E] as M1

sig E {}

pred gp [X : Exec_PTX] {

  no X.(R&W)

  no ((X.ev -> X.ev) - X.sgl)

  no X.atom

  // The execution is forbidden in M1
  not(M1/consistent[X])
    
  //M1/dead[X]
  // We don't need the deadness constraint when comparing
  // two equi-coherent architecture-level MCMs. The fact that
  // X is consistent under M2 ensures that its inconsistency
  // under M1 is not caused by the kind of bogus co-edges that
  // deadness would rule out.

  // The execution is allowed (and not faulty) in M2
  M2/consistent[X]

}

run gp for exactly 1 Exec, 6 E expect 0 // 7 min (glucose, benjamin)
run gp for exactly 1 Exec, 7 E expect 1 // 31s (glucose, benjamin)
