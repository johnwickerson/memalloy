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


  //fence_rel in imm[sb]

}

fun atom[e:PTag->E, X:Exec_H] : E->E { 
  (univ - e[rm_EV]) <: X.atom :> (univ - e[rm_EV]) }
