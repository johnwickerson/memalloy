module exec_H[E]
open exec[E]

sig Exec_H extends Exec {
  atom : E -> E // atomicity relation
}{

  // the atom relation relates a consecutively-sequenced read/write pair
  atom in (R->W) & sb & sloc
    
  no (R&W)

  // sequenced-before is total within a thread
  sthd in *sb + ~*sb

  // there are no such things as "atomic" and "non-atomic" locations
  no naL
    
}

pred wf_Exec_H[X:Exec_H, ad,cd,dd:E->E] {

  wf_Exec[X,ad,cd,dd]

  // control dependencies are defined differently in assembly
  cd.(X.sb) in cd  
}

fun atom[e:E, X:Exec_H, ad,cd,dd:E->E] : E->E { X.atom - (univ -> e) - (e -> univ) }

// Synonyms:
fun rmw[e:E, X:Exec_H, ad,cd,dd:E->E] : E->E { atom[e,X,ad,cd,dd] }

fun fre[e:E, x : Exec_H, ad,cd,dd:E->E] : E -> E {
  fr[e,x,ad,cd,dd] - sthd[e,x,ad,cd,dd]
}

fun fri[e:E, x : Exec_H, ad,cd,dd:E->E] : E -> E {
  fr[e,x,ad,cd,dd] & sthd[e,x,ad,cd,dd]
}

fun rfe[e:E, x : Exec_H, ad,cd,dd:E->E] : E -> E {
  rf[e,x,ad,cd,dd] - sthd[e,x,ad,cd,dd]
}

fun rfi[e:E, x : Exec_H, ad,cd,dd:E->E] : E -> E {
  rf[e,x,ad,cd,dd] & sthd[e,x,ad,cd,dd]
}

fun coe[e:E, x : Exec_H, ad,cd,dd:E->E] : E -> E {
  co[e,x,ad,cd,dd] - sthd[e,x,ad,cd,dd]
}

fun coi[e:E, x : Exec_H, ad,cd,dd:E->E] : E -> E {
  co[e,x,ad,cd,dd] & sthd[e,x,ad,cd,dd]
}
