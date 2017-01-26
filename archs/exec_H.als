module exec_H[E]
open exec[E]

sig Exec_H extends Exec {
  atom : E -> E // atomicity relation
}{

  // the atom relation relates a consecutively-sequenced read/write pair
  atom in (R->W) & imm[sb] & sloc
  no (R&W)

  // sequenced-before is total within a thread
  sthd in *sb + ~*sb

  // there are no such things as "atomic" and "non-atomic" locations
  no naL
    
}

fun atom[e:E, X:Exec_H] : E->E { X.atom - (univ -> e) - (e -> univ) }

fun fre[e:E, x : Exec_H] : E -> E {
  fr[e,x] - sthd[e,x]
}

fun fri[e:E, x : Exec_H] : E -> E {
  fr[e,x] & sthd[e,x]
}

fun rfe[e:E, x : Exec_H] : E -> E {
  rf[e,x] - sthd[e,x]
}

fun rfi[e:E, x : Exec_H] : E -> E {
  rf[e,x] & sthd[e,x]
}

fun coe[e:E, x : Exec_H] : E -> E {
  co[e,x] - sthd[e,x]
}

fun coi[e:E, x : Exec_H] : E -> E {
  co[e,x] & sthd[e,x]
}

//pred Atomic[e:E, x : Exec_H] {
//  is_empty[atom[e,x] & ((fre[e,x]) . (coe[e,x]))]
//}

pred storebuffering_H[x : Exec_H] {
  /*
  e1: Wx=1  e3: Wy=1
  e2: Ry=0  e4: Rx=0
  */
  some disj e1, e2, e3, e4 : E {
    x.ev = e1 + e2 + e3 + e4
    x.sb = (e1 -> e2) + (e3 -> e4)
    x.cd = none->none
    x.ad = none->none
    x.dd = none->none
    x.W = e1 + e3
    x.R = e2 + e4
    x.F = none
    x.sthd = sq[e1 + e2] + sq[e3 + e4]
    x.sloc = sq[e1 + e4] + sq[e2 + e3]
    x.rf = none->none
    x.co = none->none
    x.naL = none
  }
}

pred loadbuffering_H[x : Exec_H] {
  /*
  e1: Rx=1  e3: Ry=1
  e2: Wy=1  e4: Wx=1
  */
  some disj e1, e2, e3, e4 : E {
    x.ev = e1 + e2 + e3 + e4
    x.sb = (e1 -> e2) + (e3 -> e4)
    x.cd = none->none
    x.ad = none->none
    x.dd = none->none
    x.W = e2 + e4
    x.R = e1 + e3
    x.F = none
    x.sthd = sq[e1 + e2] + sq[e3 + e4]
    x.sloc = sq[e1 + e4] + sq[e2 + e3]
    x.rf = (e2 -> e3) + (e4 -> e1) 
    x.co = none->none
    x.naL = none
  }
}

pred iriw_H1 [x : Exec_H] {
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
    x.W = e1 + e2
    x.R = e3 + e4 + e5 + e6
    x.F = none
    x.sthd = sq[e1] + sq[e2] + sq[e3 + e4] + sq[e5 + e6]
    x.sloc = sq[e1 + e3 + e6] + sq[e2 + e4 + e5]
    x.rf = (e1 -> e3) + (e2 -> e5)
    x.co = none->none
    x.naL = none
  }
}

pred racefree[e:E, x: Exec_H] {
  no none
}

pred dead[e:E, x : Exec_H] {

  // co edges can't be changed to make consistent exec
  forced_co[e,x]

  // avoid "if(r==0)" in generated litmus test
  no_if_zero[e,x]

}

run storebuffering_H for exactly 1 Exec, 4 E
run iriw_H1 for exactly 1 Exec, 6 E
