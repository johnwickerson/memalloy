module sql16_sql16_sz_all_order[E]

/**
 * Interpret the standard as requiring a total order on serializable
 * transactions, as well as a partial order between serializable transactions
 * and all other types
 */

open ../archs/exec_SQL[E]
open sql16_base[E]
open basic[E]

fun serializability_order[e:PTag->E, X:Exec_SQL] : E -> E {
  (
   sz[e, X] <:
   (sthd[e, X]) . (rf[e, X] + fr[e, X] - sthd[e, X]) . (sthd[e, X])
   :> sz[e, X]
   ) +
  (
   sz[e, X] <:
   (sthd[e, X]) . (rf[e, X] + fr[e, X] - sthd[e, X]) . (sthd[e, X])
   :> (EV[e, X] - sz[e, X])
   ) +
  (
   (EV[e, X] - sz[e, X]) <:
   (sthd[e, X]) . (rf[e, X] + fr[e, X] - sthd[e, X]) . (sthd[e, X])
   :> sz[e, X]
   )
}

pred consistent[e:PTag->E, X:Exec_SQL] {
  base_consistent[e, X]
  is_acyclic[serializability_order[e, X]]
}

pred dead[e:PTag->E, X:Exec_SQL] { base_dead[e, X] }
