module sql16_base[E]

open ../archs/exec_SQL[E]
open basic[E]

/**
 * From the SQL Standard:
 * SQL-transaction T1 modifies a row. SQL-transaction T2 then
 * reads that row before T1 performs a COMMIT. If T1 then performs a ROLLBACK, T2
 * will have read a row that was never committed and that may thus be considered to
 * have never existed.
 *
 * If a read reads from a write, it can't have missed any other writes in that transaction
 */
pred no_illegal_dirty_read[e:PTag->E, X:Exec_SQL] {
  all W, R: EV[e, X] {
    W->R not in sthd[e, X]
    and W->R in rf[e, X]
    and R in (RC[e, X] + RR[e, X] + SER[e, X]) implies
        no R.(fr[e, X]) & W.(sthd[e, X])
  }
}

/*
 * SQL-transaction T1 reads a row. SQL-transaction T2 then modifies
 * or deletes that row and performs a COMMIT. If T1 then attempts to reread the row, it may
 * receive the modified value or discover that the row has been deleted.
 *
 * So we disallow this pattern
 */
pred no_illegal_non_repeatable_read[e:PTag->E, X:Exec_SQL] {
  // Only allow events in the same transaction to observe up to one event in
  // another transaction. I.e. don't allow them to read different values without a
  // corresponding write in their own transaction
  let target_events = (RR[e, X] + SER[e, X]) & R[e, X]
  | all e1, e2 : target_events
  | e1->e2 in (sthd[e, X] & sloc[e, X])
  // Either they read from the same event outside the transaction (or no event)
  implies e1.(~(rf[e, X])) = e2.(~(rf[e, X]))
  // or one of them reads from an in-transaction event
    or one e1.(~(rf[e, X])) & e1.(~(sb[e, X]))
    or one e2.(~(rf[e, X])) & e2.(~(sb[e, X]))
}

pred base_consistent[e:PTag->E, X:Exec_SQL] {
  no_illegal_non_repeatable_read[e, X]
  no_illegal_dirty_read[e, X]
}

pred base_dead[e:PTag->E, X:Exec_SQL] {}
