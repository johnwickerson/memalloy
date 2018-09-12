module exec_H[E]
open exec[E]

sig Exec_H extends Exec {
}{
  // there are no fence events (only fence relations)
  no F

  // control dependencies are defined differently in assembly
  cd.sb in cd
		
  // there are no single-event RMWs
  no (R&W)
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

fun mk_fence_rel[e:PTag->E, t:PTag, f:E->E, sb:E->E] : E->E {
  rm_EV_rel[e, *sb.((univ - e[t]) <: imm[f]).*sb] }
