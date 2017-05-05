module exec_H[E]
open exec[E]

sig Exec_H extends Exec {
  atom : E -> E // atomicity relation
}{

  // the atom relation relates a consecutively-sequenced read/write pair
  atom in (R->W) & sb & sloc
    
  // there are no single-event RMWs
  no (R&W)
    
  // sequenced-before is total within a thread
  sthd in *sb + ~*sb

  // there are no such things as "atomic" and "non-atomic" locations
  no NAL

  // control dependencies are defined differently in assembly
  cd.sb in cd 
    
}

fun atom[e:E, X:Exec_H] : E->E { X.atom - (univ -> e) - (e -> univ) }
