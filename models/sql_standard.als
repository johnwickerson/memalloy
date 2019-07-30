module sql_standard[E]

open ../archs/exec_SQL[E]
open basic[E]

/**
 * From the SQL Standard:
 * SQL-transaction T1 modifies a row. SQL-transaction T2 then
 * reads that row before T1 performs a COMMIT. If T1 then performs a ROLLBACK, T2
 * will have read a row that was never committed and that may thus be considered to
 * have never existed.
 *
 * The commit must happen before T2.
 */
fun dirty_read_hb[e:PTag->E, X:Exec_SQL] : E->E {
  let write_rf_to_rc = (rf[e, X] - sthd[e, X]) :> (rc[e, X] + rr[e, X] + sz[e, X])
    , commit_before_rc_read = ~(commit_of[X]) . write_rf_to_rc
  | commit_before_rc_read
}

/*
 * SQL-transaction T1 reads a row. SQL-transaction T2 then modifies
 * or deletes that row and performs a COMMIT. If T1 then attempts to reread the row, it may
 * receive the modified value or discover that the row has been deleted.
 *
 * So we disallow this pattern
 */
pred No_illegal_non_repeatable_read[e:PTag->E, X:Exec_SQL] {
  // Only allow events in the same transaction to observe up to one event in
  // another transaction. I.e. don't allow them to read different values without a
  // corresponding write in their own transaction
  let target_events = rr[e, X] + sz[e, X]
  | all e1, e2 : target_events
  | e1->e2 in (sthd[e, X] & sloc[e, X])
  implies lone e1.(~(rf[e, X])) + e2.(~(rf[e, X])) - (e1 + e2).(sthd[e, X])
}

fun hb[e:PTag->E, X:Exec_SQL] : E->E {
  // sb => hb (no reordering events in a transaction)
  sb[e, X] +
  dirty_read_hb[e, X]
}

pred Causality [e:PTag->E, X:Exec_SQL] {
  is_acyclic[(hb[e,X]) + (po[e,X]) + (rf[e,X]) + (fr[e, X])]
}

pred consistent[e:PTag->E, X:Exec_SQL] {
  No_illegal_non_repeatable_read[e, X]
  Causality[e, X]
}

pred dead[e:PTag->E, X:Exec_SQL] {}
