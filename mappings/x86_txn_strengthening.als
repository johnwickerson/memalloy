open ../archs/exec_x86[E]
open ../models/x86tso_txn[E] as M

module x86_txn_strengthening[E]

pred apply_map[X,X':Exec_X86] { 

  X.EV = X'.EV
  
  X.R = X'.R
  X.W = X'.W
  X.F = X'.F
 
  X.sb = X'.sb
  X.cd = X'.cd
  X.ad = X'.ad
  X.dd = X'.dd
  X.rf = X'.rf
  X.co = X'.co
  X.sloc = X'.sloc
  X.sthd = X'.sthd
  X.atom = X'.atom

  X.MFENCE = X'.MFENCE

  X.stxn in X'.stxn

}

pred p[X,X':Exec_X86] {
  withoutinit[X]
  withoutinit[X']

  not (M/consistent[none->none, X])
  //M/dead[none->none, X]

  M/consistent[none->none, X']

  apply_map[X,X']

  X.EV = E
}

run p for 2 Exec, 4 E
