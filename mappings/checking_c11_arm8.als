open ../models/aarch64_txn[HE] as M1
open ../models/c11_lahav[SE] as N1
open ../mappings/fences_as_relations/c11_arm8[SE,HE] as mapping

sig SE, HE {}

pred gp [X:Exec_C, Y:Exec_Arm8, map:SE->HE] {

  withoutinit[X]
  withoutinit[Y]

  not(N1/consistent[none->none,X])
  N1/dead[none->none,X]

  M1/consistent[none->none,Y]

  // We have a valid application of the mapping
  apply_map[X, Y, map]

}

run gp for exactly 1 M1/Exec, exactly 1 N1/Exec, 5 SE, 5 HE, 3 Int // no soln on benjamin in 8 min (488s)
run gp for exactly 1 M1/Exec, exactly 1 N1/Exec, 6 SE, 6 HE, 3 Int // no soln on benjamin in 20h (71650s)
