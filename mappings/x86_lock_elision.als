open ../archs/exec_x86L[E]
open ../models/x86tso_txn[E] as M

module x86_lock_elision[E]

pred apply_map[X,X':Exec_X86L, map:E->E] { 

  map in (X.EV one-> X'.EV)

  // no critical sections remain
  no X'.(LkT + UkT + LkN + UkN)

  // suppose we don't have transactions in the source program
  no X.stxn

  // events in the same critical section (where the critical
  // section is one that is to be transactionalised), map to events 
  // in the same transaction.
  (X.EV - X.UkT) <: scst[X] :> (X.EV - X.UkT) = map.(X'.stxn).~map

  // the set of lock-related events introduced by the mapping
  let L = map[X.(LkT + UkT + LkN + UkN)] {

    // Mildly interesting observation: transactional memory models
    // are not monotonic w.r.t. adding sequencing edges

    X'.sthd = ~map.(X.sthd).map

    all e1, e2 : X.EV |
      (some e1.map and some e2.map) implies
      ((e1 -> e2 in X.sb) iff (map[e1] -> map[e2] in X'.sb))

    X.ad =   map.((X'.EV - L) <: (X'.ad)   :> (X'.EV - L)).~map
    X.cd =   map.((X'.EV - L) <: (X'.cd)   :> (X'.EV - L)).~map
    X.dd =   map.((X'.EV - L) <: (X'.dd)   :> (X'.EV - L)).~map
    X.atom = map.((X'.EV - L) <: (X'.atom) :> (X'.EV - L)).~map
    X.rf =   map.((X'.EV - L) <: (X'.rf)   :> (X'.EV - L)).~map
    X.co =   map.((X'.EV - L) <: (X'.co)   :> (X'.EV - L)).~map

    X.R     = (X'.R     - L).~map
    X.W     = (X'.W     - L).~map
    X.F     = (X'.F     - L).~map
    X.MFENCE = (X'.MFENCE - L).~map

    // lock-related events in X' are on the same location
    X'.sloc = (L -> L) + ((X'.EV - L) <: ~map.(X.sloc).map :> (X'.EV - L))

    // non-call events map to non-library events
    all e : X.EV - X.LkT - X.LkN - X.UkT - X.UkN {
	  one e.map
    }

    // lock() to be transactionalised
	all e : X.LkT | some e1 : X'.EV {
      e.map = e1
      e1 in X'.R // e1 is a lock-related read event
	  e1 not in ran[X'.rf] // e1 sees the lock is not taken
    }

	// unlock() to be transactionalised
    all e : X.UkT | no e.map

    // lock() to be kept as a normal lock
    all e : X.LkN | some disj e1, e2, e3 : X'.EV {
      e.map = e1 + e2 + e3
      (e1 -> e2) in imm[X'.sb] & X'.cd // e1 controls e2
      (e2 -> e3) in imm[X'.sb] & X'.atom // e2 and e3 form an RMW pair 
      e1+e2 in X'.R // e1 and e2 are lock-related reads
      e3 in X'.W // e2 is a lock-related write
    }

    // unlock() to be kept as a normal unlock
    all e : X.UkN | some e1 : X'.EV {
 	  e.map = e1
      e1 in X'.W // eL is a write
    }

  }   
}

pred consistent_critical_sections[X:Exec_X86L] {
  // no (po|com)-cycles among critical sections
  let com = X.rf + X.co + fr[none->none,X] |
  let scs = scst[X] + scsl[X] |
  is_acyclic[ scs . (X.sb + com - scs) . scs ]
}

pred p[X,X':Exec_X86L, map:E->E] {
  withoutinit[X]
  withoutinit[X']

 not {
    M/consistent[none->none, X]
    consistent_critical_sections[X]
  }

  //M/dead[none->none, X]

  M/consistent[none->none, X']

  apply_map[X,X',map]

}

run p for 2 Exec, 8 E