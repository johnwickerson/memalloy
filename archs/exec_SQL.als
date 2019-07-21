module exec_SQL[E]
open exec[E]

sig Exec_SQL extends Exec {
  rc : set E,       // Events in read-committed transactions
  rr : set E,       // Events in repeatable-read transactions
  sz : set E,       // Events in serializable transactions

  sstmt : E->E,     // Same statement
} {
  // No dependencies
  no ad
  no cd
  no dd

  // No fences
  no F

  // Events can be of only one type
  disj[R, W]

  // Events can be in only one isolation level
  disj[rc, rr, sz]

  // All events are in some isolation level
  EV in (rc + rr + sz)

  // Events in the same thread (transaction) are in the same isolation level
  rc.sthd in rc
  rr.sthd in rr
  sz.sthd in sz

  // Only events in the same thread can come from the same statement
  sstmt in sthd

  // sstmt is an equivalence relation on EV
  is_equivalence[sstmt, EV - IW]
}

one sig rm_rc extends PTag {}
one sig rm_rr extends PTag {}
one sig rm_sz extends PTag {}

fun rc[e:PTag->E, X:Exec_SQL] : set E {
  X.rc - e[rm_EV] - e[rm_rc]
}

fun rr[e:PTag->E, X:Exec_SQL] : set E {
  X.rr - e[rm_EV] - e[rm_rr]
}

fun sz[e:PTag->E, X:Exec_SQL] : set E {
  X.sz - e[rm_EV] - e[rm_sz]
}
