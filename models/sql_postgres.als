module sql_postgres[E]

open ../archs/exec_SQL[E]
open basic[E]

/**
 * In every transaction level, events can only read from transactions that
 * commited before their query began
 *
 * This deviates from the standard. The standard only requires the commit to
 * happen before the reading event. Postgres required it before the start of the
 * reading **query**. The current version considers each event to be in its own
 * query
 *
 * This relation creates hb-edges wherever a transaction needs to commit before
 * the beginning of another one that reads from it
 */
fun read_commited_hb[e:PTag->E, X:Exec_SQL] : E->E {
    ~(X.commit_of) . (rf[e, X] - sthd[e, X])
}

/**
 * In transaction levels above repeatable read, events can only read from
 * transactions that commited before their transaction began
 *
 * This relation creates hb-edges wherever a transaction needs to commit before
 * the beginning of another one that reads from it
 */
fun repeatable_read_hb[e:PTag->E, X:Exec_SQL] : E->E {
    ~(X.commit_of) . (rf[e, X] - sthd[e, X]) . (X.transaction_begin) :> (X.rr+X.sz)
}

fun hb[e:PTag->E, X:Exec_SQL] : E->E {
  sb[e, X] +
  read_commited_hb[e, X] +
  repeatable_read_hb[e, X]
}

pred Causality [e:PTag->E, X:Exec_SQL] {
  is_acyclic[(hb[e,X]) + (po[e,X]) + (rf[e,X]) + (fr[e, X])]
}

pred consistent[e:PTag->E, X:Exec_SQL] {
  Causality[e, X]
}

pred dead[e:PTag->E, X:Exec_SQL] {}

