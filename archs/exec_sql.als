module exec_sql[E]
open exec[E]

sig Exec_Sql extends Exec {
  C : set E,        // Commit events
  RB : set E,       // Rollback events

  rc : set E,       // Events in read-committed transactions
  rr : set E,       // Events in repeatable-read transactions
  sz : set E,       // Events in serializable transactions

  sstmt : E->E
} {
  // Commits and rollbacks are events
  C + RB in EV

  // No dependencies
  no ad
  no cd
  no dd

  // Events can be of only one type
  disj[R, W, C, RB]

  // Events can be in only one isolation level
  disj[rc, rr, sz]

  // All events are in some isolation level
  EV in (rc + rr + sz)

  // Events in the same thread (transaction) are in the same isolation level
  rc.sthd in rc
  rr.sthd in rr
  sz.sthd in sz

  // last event == commit or rollback
  all e : E | no e.sb iff e in (C + RB)

  // Only events in the same thread can come from the same statement
  sstmt in sthd

  // sstmt is an equivalence relation on EV
  is_equivalence[sstmt, EV - IW]
}

pred interesting[X : Exec_Sql] {
  withoutinit[X]

  some X.R
  some X.W
}

run interesting for
exactly 1 Exec,
exactly 1 Exec_Sql,
exactly 5 E
