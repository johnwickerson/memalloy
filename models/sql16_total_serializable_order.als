module sql16_total_serializable_order[E]

open ../archs/exec_SQL[E]
open sql16_base[E]
open basic[E]

pred order_exists[e:PTag->E, X:Exec_SQL] {
  let SER_E = SER[e, X] | some order : SER_E -> SER_E {
    totalOrder[order, SER[e, X]]
    (po[e, X] :> SER[e, X]) in order
    is_acyclic[order + rf[e, X] + fr[e, X] - iden]
  }
}

pred consistent[e:PTag->E, X:Exec_SQL] {
  base_consistent[e, X]
  order_exists[e, X]
}

