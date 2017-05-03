pred hint[X:Exec, ad,cd,dd:E->E] {
  some disj E5,E4,E3,E2,E1,E0 : E {
    X.naL = none
    X.IW = none
    X.F = none
    X.R = E5+E4
    X.W = E3+E2+E1+E0
    X.ev = E5+E4+E3+E2+E1+E0
    X.sc = none
    X.rel = E3+E2+E1+E0
    X.acq = E5+E4
    X.A = E5+E4+E3+E2+E1+E0
    X.co = (E3->E0)+(E1->E2)
    X.rf = (E2->E5)+(E0->E4)
    X.sloc = (E5->E5)+(E5->E2)+(E5->E1)+(E4->E4)+(E4->E3)+(E4->E0)+(E3->E4)+(E3->E3)+(E3->E0)+(E2->E5)+(E2->E2)+(E2->E1)+(E1->E5)+(E1->E2)+(E1->E1)+(E0->E4)+(E0->E3)+(E0->E0)
    X.sthd = (E5->E5)+(E5->E1)+(E5->E0)+(E4->E4)+(E4->E3)+(E4->E2)+(E3->E4)+(E3->E3)+(E3->E2)+(E2->E4)+(E2->E3)+(E2->E2)+(E1->E5)+(E1->E1)+(E1->E0)+(E0->E5)+(E0->E1)+(E0->E0)
    cd = none->none
    dd = none->none
    ad = none->none
    X.sb = (E3->E4)+(E2->E4)+(E2->E3)+(E1->E5)+(E0->E5)+(E0->E1)
  }
}

