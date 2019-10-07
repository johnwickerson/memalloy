module exec_SQL[E]
open exec[E]

sig Exec_SQL extends Exec {
  C : set E,        // Commit events

  RU : set E,       // Events in read-uncommitted transactions
  RC : set E,       // Events in read-committed transactions
  RR : set E,       // Events in repeatable-read transactions
  SER : set E,       // Events in serializable transactions
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
  disj[RU, RC, RR, SER]

  // All events are in some isolation level
  EV in (RU + RC + RR + SER)

  // Events in the same thread (transaction) are in the same isolation level
  RU.sthd in RU
  RC.sthd in RC
  RR.sthd in RR
  SER.sthd in SER

   // last event == commit
  all e : E | no e.sb iff e in C

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
one sig rm_RU extends PTag {}
one sig rm_RC extends PTag {}
one sig rm_RR extends PTag {}
one sig rm_SER extends PTag {}

fun C[e:PTag->E, X:Exec_SQL] : set E {
  X.C - e[rm_EV] - e[rm_C]
}

fun RU[e:PTag->E, X:Exec_SQL] : set E {
  X.RU - e[rm_EV] - e[rm_RU]
}

fun RC[e:PTag->E, X:Exec_SQL] : set E {
  X.RC - e[rm_EV] - e[rm_RC]
}

fun RR[e:PTag->E, X:Exec_SQL] : set E {
  X.RR - e[rm_EV] - e[rm_RR]
}

fun SER[e:PTag->E, X:Exec_SQL] : set E {
  X.SER - e[rm_EV] - e[rm_SER]
}
