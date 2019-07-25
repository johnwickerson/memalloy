module exec_SQL[E]
open exec[E]

sig Exec_SQL extends Exec {
  C : set E,        // Commit events

  ru : set E,       // Events in read-uncommitted transactions
  rc : set E,       // Events in read-committed transactions
  rr : set E,       // Events in repeatable-read transactions
  sz : set E,       // Events in serializable transactions

  sstmt : E->E,     // Same statement
} {
  // No dependencies
  no ad
  no cd
  no dd

  // No atomics
  no atom

  // Commits are fences
  /* TODO modify memalloy so that it allows commits even if they are not in R, W or F */
  C = F

  // Events can be of only one type
  disj[R, W, C]

  // Reads, writes and commits are the only types of events
  (R + W + C) = EV

  // Events can be in only one isolation level
  disj[ru, rc, rr, sz]

  // All events are in some isolation level
  EV in (ru + rc + rr + sz)

  // Events in the same thread (transaction) are in the same isolation level
  ru.sthd in ru
  rc.sthd in rc
  rr.sthd in rr
  sz.sthd in sz

   // last event == commit
  all e : E | no e.sb iff e in C

  // Only events in the same thread can come from the same statement
  sstmt in sthd

  // sstmt is an equivalence relation on EV
  is_equivalence[sstmt, EV - IW]

  // Intra-thread read causality
  is_acyclic[rf + sb]
}

/** Relate every event to the first event in its transaction */
fun transaction_begin[X:Exec_SQL] : E->E {
  ~(X.sb) :> (X.EV - X.sb[X.EV])
}

fun commit_of[X:Exec_SQL] : E->E {
  X.sb :> X.C
}

one sig rm_C extends PTag {}
one sig rm_ru extends PTag {}
one sig rm_rc extends PTag {}
one sig rm_rr extends PTag {}
one sig rm_sz extends PTag {}

fun C[e:PTag->E, X:Exec_SQL] : set E {
  X.C - e[rm_EV] - e[rm_C]
}

fun ru[e:PTag->E, X:Exec_SQL] : set E {
  X.ru - e[rm_EV] - e[rm_ru]
}

fun rc[e:PTag->E, X:Exec_SQL] : set E {
  X.rc - e[rm_EV] - e[rm_rc]
}

fun rr[e:PTag->E, X:Exec_SQL] : set E {
  X.rr - e[rm_EV] - e[rm_rr]
}

fun sz[e:PTag->E, X:Exec_SQL] : set E {
  X.sz - e[rm_EV] - e[rm_sz]
}
