module sql16_sep_sz[E]

/**
 * Interpret the standard as requiring a total order on serializable
 * transactions
 */

open ../archs/exec_SQL[E]
open sql16_base[E]
open basic[E]

fun serializable_hb[e:PTag->E, X:Exec_SQL] : E->E {
  sz[e, X] <:
  ~(commit_of[X]) . (rf[e, X] + fr[e, X]) . ~(transaction_begin[X])
  :> sz[e, X]
}

fun hb[e:PTag->E, X:Exec_SQL] : E->E {
  base_hb[e, X] +
  serializable_hb[e, X]
}

pred causality [e:PTag->E, X:Exec_SQL] {
  is_acyclic[(hb[e,X]) + (po[e,X]) + (rf[e,X]) + (fr[e, X])]
}

pred consistent[e:PTag->E, X:Exec_SQL] {
  causality[e, X]
}

pred dead[e:PTag->E, X:Exec_SQL] { base_dead[e, X] }
