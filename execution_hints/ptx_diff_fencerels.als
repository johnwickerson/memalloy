pred hint[X:Exec_PTX] {
// Test "WWC+membar.cta+membar.gl":
//  e1: W y 2 ||  e2: R y 2   |  e4: R x 1
//            ||  membar.cta  |  membar.gl
//            ||  e3: W x 1   |  e5: W y 1
  some disj e1,e2,e3,e4,e5 : E {
    X.ev = e1+e2+e3+e4+e5
    X.W = e1+e3+e5
    X.R = e2+e4
    X.sb = (e2->e3) + (e4->e5)
    X.ad = none -> none
    X.cd = none -> none
    X.dd = none -> none
    X.sloc = sq[e1+e2+e5] + sq[e3+e4]
    X.sthd = sq[e1] + sq[e2+e3] + sq[e4+e5]
    X.scta = sq[e1] + sq[e2+e3  +    e4+e5]
    X.sgl  = sq[e1  +    e2+e3  +    e4+e5]
    X.membar_cta = e2->e3 + membar_gl
    X.membar_gl = e4->e5
    X.membar_sys = none->none
    X.rf = (e1->e2) + (e3->e4)
    X.co = (e5->e1)
  }
}
