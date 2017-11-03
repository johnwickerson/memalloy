open ../archs/exec_C[E]
open ../models/c11_txn[E] as M

module c11_txn_strengthening[E]

pred apply_map[X,X':Exec_C] { 

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

  X.A = X'.A
  X.ACQ = X'.ACQ
  X.REL = X'.REL
  X.SC = X'.SC
  X.NAL = X'.NAL
    
  X.stxn in X'.stxn

}

pred p[X,X':Exec_C] {
  withoutinit[X]
  withoutinit[X']

  not (M/consistent[none->none, X])
  //M/dead[none->none, X]

  M/consistent[none->none, X']

  apply_map[X,X']

  X.EV = E
}

run p for 2 Exec, 3 E
