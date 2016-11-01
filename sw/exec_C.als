module exec_C[E]
open ../exec[E]

sig Exec_C extends Exec {
  A : set E,            // atomic events
  acq, rel, sc : set E, // acquire, release, sc events
  failedCAS : set E,    // reads corresponding to failed CAS instructions
}{

  A in ev

  // acquires, releases, and SC operations are all atomic
  acq + rel + sc in A

  // RMWs and fences are atomic
  (F + (R & W)) in A
    
  // sc reads can acquire
  (R & sc) in acq

  // only reads and fences can acquire
  acq in (R + F)
    
  // sc writes can release
  (W & sc) in rel

  // only writes and fences can release
  rel in (W + F)
    
  // sc fences can acquire and release
  (F & sc) in (acq & rel)

  // atomic events do not access non-atomic locations
  no (A & naL)

  // non-atomic reads do not access atomic locations
  R-A in naL

  // failedCAS events are reads
  failedCAS in R

}

pred wf_s[x : Exec_C, s : E -> E] { 

  // s is restricted to sc events
  s in x.sc -> x.sc
    
  // s is acylic
  is_acyclic[s]
    
  // s is transitive
  transitive[s]

  // s is a strict total relation on sc events
  (all e, e' : (x.sc) | (e != e') iff (e -> e' in (s + ~s)))
}

/*************************/
/*      TESTS            */
/*************************/

pred messagepassing_C[x : Exec_C] {
  /*
  e1: Wx=1   e3: Ry=1
  e2: Wy=1   e4: Rx=0
  */
  some disj e1, e2, e3, e4 : E {
    x.ev = e1 + e2 + e3 + e4
    x.sb = (e1 -> e2) + (e3 -> e4)
    x.cd = (e3 -> e4)
    x.ad = none->none
    x.dd = none->none
    x.A = e2 + e3
    x.W = e1 + e2
    x.R = e3 + e4
    x.rel = e2
    x.acq = e3
    x.sc = none
    x.F = none
    x.sthd = sq[e1 + e2] + sq[e3 + e4]
    x.sloc = sq[e1 + e4] + sq[e2 + e3]
    x.naL = e1 + e4
    x.rf = (e2 -> e3)
    x.co = none->none
  }
}

pred storebuffering_C [x : Exec_C] {
  /*
  e1: Wx=1  e3: Wy=1
  e2: Ry=0  e4: Rx=0
  */
  some disj e1, e2, e3, e4 : E {
    x.ev = e1 + e2 + e3 + e4
    x.sb = (e1 -> e2) + (e3 -> e4)
    x.A = e1 + e2 + e3 + e4
    x.W = e1 + e3
    x.R = e2 + e4
    x.F = none
    x.sthd = sq[e1 + e2] + sq[e3 + e4]
    x.sloc = sq[e1 + e4] + sq[e2 + e3]
    x.rf = none->none
    x.co = none->none
    x.naL = none
    x.acq = e2 + e4
    x.rel = e1 + e3
    x.sc = e1 + e2 + e3 + e4
  }
}

pred iriw_C [x : Exec_C] {
  /*
  e1: Wx=1  e2: Wy=1  e3: Rx=1  e5: Ry=1
                      e4: Ry=0  e6: Rx=0
  */
  some disj e1, e2, e3, e4, e5, e6 : E {
    x.ev = e1 + e2 + e3 + e4 + e5 + e6
    x.sb = (e3 -> e4) + (e5 -> e6)
    x.ad = none->none
    x.cd = none->none
    x.dd = none->none
    x.A = x.ev
    x.W = e1 + e2
    x.R = e3 + e4 + e5 + e6
    x.F = none
    x.sthd = sq[e1] + sq[e2] + sq[e3 + e4] + sq[e5 + e6]
    x.sloc = sq[e1 + e3 + e6] + sq[e2 + e4 + e5]
    x.rf = (e1 -> e3) + (e2 -> e5)
    x.co = none->none
    x.naL = none
    x.acq = x.R
    x.rel = x.W
    x.sc = x.ev
  }
}

run messagepassing_C for exactly 1 Exec, 4 E
run storebuffering_C for exactly 1 Exec, 4 E
run iriw_C for exactly 1 Exec, 6 E
