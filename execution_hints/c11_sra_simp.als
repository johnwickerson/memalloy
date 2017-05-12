pred c11_sra_simp[X:Exec] {
  some disj E5,E4,E3,E2,E1,E0 : E {
    X.NAL = none
    X.IW = none
    X.F = none
    X.R = E5+E4
    X.W = E3+E2+E1+E0
    X.EV = E5+E4+E3+E2+E1+E0
    X.SC = none
    X.REL = E3+E2+E1+E0
    X.ACQ = E5+E4
    X.A = E5+E4+E3+E2+E1+E0
    X.co = (E3->E0)+(E1->E2)
    X.rf = (E2->E5)+(E0->E4)
    X.sloc = (E5->E5)+(E5->E2)+(E5->E1)+(E4->E4)+(E4->E3)+(E4->E0)+(E3->E4)+(E3->E3)+(E3->E0)+(E2->E5)+(E2->E2)+(E2->E1)+(E1->E5)+(E1->E2)+(E1->E1)+(E0->E4)+(E0->E3)+(E0->E0)
    X.sthd = (E5->E5)+(E5->E1)+(E5->E0)+(E4->E4)+(E4->E3)+(E4->E2)+(E3->E4)+(E3->E3)+(E3->E2)+(E2->E4)+(E2->E3)+(E2->E2)+(E1->E5)+(E1->E1)+(E1->E0)+(E0->E5)+(E0->E1)+(E0->E0)
    X.cd = none->none
    X.dd = none->none
    X.ad = none->none
    X.sb = (E3->E4)+(E2->E4)+(E2->E3)+(E1->E5)+(E0->E5)+(E0->E1)
  }
}

