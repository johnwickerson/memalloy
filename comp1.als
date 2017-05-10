open models/totalsb[E] as M1
open models/c11_normws[E] as M2
open models/c11_partial[E] as M3
open models/c11_lidbury[E] as N1
sig E {}

pred gp [X:Exec_C] {

  withoutinit[X]

  not(N1/consistent[none,X])
  N1/dead[none,X]
  M1/consistent[none,X]
  M2/consistent[none,X]
  M3/consistent[none,X]

  not (some e : X.EV {
    not(N1/consistent[e,X])
    N1/dead[e,X]
    M1/consistent[e,X]
    M2/consistent[e,X]
    M3/consistent[e,X]
  })
    
  not hint1[X]
  not hint2[X]
  not hint3[X]
  not hint4[X]
  not hint5[X]
  not hint6[X]
  not hint7[X]
  not hint8[X]
  not hint9[X]
  not hint10[X]
  not hint11[X]
  not hint12[X]
  not hint13[X]
  not hint14[X]
  not hint15[X]
  not hint16[X]
  not hint17[X]
  not hint18[X]
  not hint19[X]
  not hint20[X]
  not hint21[X]
  not hint22[X]
  not hint23[X]
  not hint24[X]
  not hint25[X]
  not hint26[X]
  not hint27[X]
  not hint28[X]
  not hint29[X]
  not hint30[X]
  not hint31[X]
  not hint32[X]
  not hint33[X]
  not hint34[X]
  not hint35[X]
  not hint36[X]
  not hint37[X]
  not hint38[X]
  not hint39[X]
  not hint40[X]
  not hint41[X]
  not hint42[X]
  //not hint43[X]
  //not hint44[X]
  //not hint45[X]
  //not hint46[X]
  //not hint47[X]
  //not hint48[X]
  //not hint49[X]
}

run gp for 1 Exec, 4 E, 3 Int

pred hint42[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2 in X.SC
    E2+E1+E0 in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E0->E1)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E2->E0)+(E1->E3)
  }
}

pred hint41[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    none in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)+(E1->E3)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint40[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2 in X.SC
    E3+E2 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E3->E1)+(E2->E0)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E1->E2)+(E0->E3)
  }
}

pred hint39[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E3 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)+(E1->E3)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint38[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E3+E2 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)+(E1->E3)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint37[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2+E1 in X.SC
    E2+E1 in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E1->E2)+(E0->E3)
  }
}

pred hint36[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2+E1 in X.SC
    E2+E1+E0 in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E1->E2)+(E0->E3)
  }
}

pred hint35[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    none in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E1->E3)+(E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint34[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    none in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E1->E3)+(E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint33[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E1->E3)+(E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint32[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E3+E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)+(E1->E3)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint31[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E1->E3)+(E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint30[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    E3+E1 in X.SC
    E1+E0 in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E2->E3)+(E1->E0)
  }
}

pred hint29[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E3+E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E1->E3)+(E0->E2)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint28[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    E3+E1 in X.SC
    E1 in X.REL
    E3+E2 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E2->E3)+(E1->E0)
  }
}


pred hint27[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E3+E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)+(E1->E3)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint26[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2+E1 in X.SC
    E2+E1 in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E0->E2)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E2->E3)+(E1->E0)
  }
}

pred hint25[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2+E1 in X.SC
    E2+E1+E0 in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E0->E2)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E2->E3)+(E1->E0)
  }
}

pred hint24[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2 in X.SC
    E3+E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E3)+(E1->E0)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E1)+(E0->E2)
  }
}

pred hint1[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    E1 in X.SC
    E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E1->E3)+(E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint2[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E2+E1+E0 in X.SC
    E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E0->E1)
    X.rf = (E2->E3)
    X.sloc = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E1->E2)
  }
}

pred hint3[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    E3+E1+E0 in X.SC
    E1+E0 in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E2->E3)+(E1->E0)
  }
}

pred hint4[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2+E1 in X.SC
    E3+E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E3->E0)+(E2->E1)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E1->E3)+(E0->E2)
  }
}

pred hint5[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2+E1 in X.SC
    E3+E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E3->E0)+(E2->E1)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E1->E3)+(E0->E2)
  }
}

pred hint6[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E2+E1 in X.SC
    E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = (E1->E3)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint7[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2 in X.SC
    E3+E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E3->E0)+(E2->E1)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E1->E3)+(E0->E2)
  }
}

pred hint8[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2 in X.SC
    E3+E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E3->E0)+(E2->E1)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E1->E3)+(E0->E2)
  }
}

pred hint9[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E2+E1 in X.SC
    E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = (E1->E3)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint10[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = (E1->E3)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint11[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = (E1->E3)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint12[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    E1 in X.SC
    E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E1->E3)+(E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint13[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    E2+E1 in X.SC
    E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E0->E2)
    X.rf = (E1->E3)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint14[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2 in X.SC
    E3+E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)+(E1->E3)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint15[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    E2 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = (E1->E3)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint16[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    E3 in X.SC
    none in X.REL
    E3 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E1->E3)+(E0->E2)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E0)+(E2->E1)
  }
}

pred hint17[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2 in X.SC
    E3+E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)+(E1->E3)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint18[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    none in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = (E1->E3)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    (E3->E2) in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint19[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3
    X.W = E2+E1+E0
    X.EV = E3+E2+E1+E0
    none in X.SC
    none in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)
    X.rf = (E1->E3)
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint20[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3 in X.SC
    E3+E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E2->E0)+(E1->E3)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E0->E1)
  }
}

pred hint21[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2+E1 in X.SC
    E3+E2+E1 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E3->E2)+(E0->E1)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E1)+(E2->E2)+(E2->E0)+(E1->E3)+(E1->E1)+(E0->E2)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E2->E0)+(E1->E3)
  }
}

pred hint22[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E3+E2
    X.W = E1+E0
    X.EV = E3+E2+E1+E0
    E3+E2+E1 in X.SC
    E1 in X.REL
    E3+E2 in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = none->none
    X.rf = (E0->E3)
    X.sloc = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E3->E2)+(E1->E0)
  }
}

pred hint23[X:Exec] {
  some disj E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = none
    X.W = E3+E2+E1+E0
    X.EV = E3+E2+E1+E0
    E3 in X.SC
    E3+E2+E1+E0 in X.REL
    none in X.ACQ
    E3+E2+E1+E0 in X.A
    X.co = (E3->E0)+(E1->E2)
    X.rf = none->none
    X.sloc = (E3->E3)+(E3->E0)+(E2->E2)+(E2->E1)+(E1->E2)+(E1->E1)+(E0->E3)+(E0->E0)
    X.sthd = (E3->E3)+(E3->E2)+(E2->E3)+(E2->E2)+(E1->E1)+(E1->E0)+(E0->E1)+(E0->E0)
    none->none in X.dd
    none->none in X.cd
    none->none in X.ad
    X.sb = (E2->E3)+(E0->E1)
  }
}
