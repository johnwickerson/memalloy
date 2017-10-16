open ../archs/exec_ppc[E]
open ../models/ppc_txn[E] as M

module ppc_strengthening[E]

pred apply_map[X,X':Exec_PPC] { 

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

  X.stxn = X'.stxn

  X.ISYNC = X'.ISYNC
  X.LWSYNC = X'.LWSYNC
  X.SYNC = X'.SYNC

  X.stxn in X'.stxn

}

pred p[X,X':Exec_PPC] {
  withoutinit[X]
  withoutinit[X']

  not (M/consistent[none->none, X])
  M/dead[none->none, X]

  M/consistent[none->none, X']

  apply_map[X,X']

  X.EV = E
}

run p for 2 Exec, 6 E
