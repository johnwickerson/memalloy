pred hint[X:Exec_PTX] {
// Test "WWC+membar.cta+membar.gl":
//  e1: W y 2 ||  e2: R y 2       |  e5: R x 1
//            ||  e3: membar.cta  |  e6: membar.gl
//            ||  e4: W x 1       |  e7: W y 1
  some disj e1,e2,e3,e4,e5,e6,e7 : E {
    X.ev = e1+e2+e3+e4+e5+e6+e7
    X.W = e1+e4+e7
    X.R = e2+e5
    X.sb = ^((e2->e3) + (e3->e4)) + ^((e5->e6) + (e6->e7))
    X.ad = none -> none
    X.cd = none -> none
    X.dd = none -> none
    X.sloc = sq[e1+e2+e7] + sq[e4+e5]
    X.sthd = sq[e1] + sq[e2+e3+e4] + sq[e5+e6+e7]
    X.scta = sq[e1] + sq[e2+e3+e4  +    e5+e6+e7]
    X.sgl  = sq[e1  +    e2+e3+e4  +    e5+e6+e7]
    X.membar_cta = e3
    X.membar_gl = e6
    X.membar_sys = none
    X.rf = (e1->e2) + (e4->e5)
    X.co = (e7->e1)
  }
}
