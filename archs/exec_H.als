module exec_H[E]
open exec[E]

sig Exec_H extends Exec {
  atom : E -> E // atomicity relation
}{

  // the atom relation relates a consecutively-sequenced read/write pair
  atom in (R->W) & sb & sloc
    
  // there are no single-event RMWs
  no (R&W)

  // there are no fence events (only fence relations)
  no F
    
  // sequenced-before is total within a thread
  sthd in *sb + ~*sb

  // there are no such things as "atomic" and "non-atomic" locations
  no naL

  // control dependencies are defined differently in assembly
  cd.sb in cd 
    
}

fun atom[e:E, X:Exec_H] : E->E { X.atom - (univ -> e) - (e -> univ) }

fun fre[e:E, X:Exec_H] : E->E { fr[e,X] - sthd[e,X] }
fun fri[e:E, X:Exec_H] : E->E { fr[e,X] & sthd[e,X] }
fun rfe[e:E, X:Exec_H] : E->E { rf[e,X] - sthd[e,X] }
fun rfi[e:E, X:Exec_H] : E->E { rf[e,X] & sthd[e,X] }
fun coe[e:E, X:Exec_H] : E->E { co[e,X] - sthd[e,X] }
fun coi[e:E, X:Exec_H] : E->E { co[e,X] & sthd[e,X] }
