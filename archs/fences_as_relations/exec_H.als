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
  no NAL

  // control dependencies are defined differently in assembly
  cd.sb in cd
}

pred is_fence_rel[fence_rel:E->E, sb:E->E] {
    
  // Consistent with program order
  fence_rel in sb

  // Preserved by pre- or post-composition with program order  
  *sb . fence_rel . *sb in fence_rel

  // Can be traced back to an immediate sb-pair
  no ((sb - fence_rel) . (sb - fence_rel)) & fence_rel
}

fun atom[e:E, X:Exec_H] : E->E { X.atom - (univ -> e) - (e -> univ) }
