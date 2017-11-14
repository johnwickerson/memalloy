open ../archs/exec_arm8L[E]
open ../models/aarch64_txn[E] as M

module arm8_lock_elision[E]

pred apply_map[X,X':Exec_Arm8L, map:E->E] { 

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
    X.DMBST = (X'.DMBST - L).~map
    X.DMBLD = (X'.DMBLD - L).~map
    X.ISB   = (X'.ISB   - L).~map
    X.SCACQ = (X'.SCACQ - L).~map
    X.SCREL = (X'.SCREL - L).~map 

    // lock-related events in X' are on the same location
    X'.sloc = (L -> L) + ((X'.EV - L) <: ~map.(X.sloc).map :> (X'.EV - L))

    // non-call events map to non-library events
    all e : X.EV - X.LkT - X.LkN - X.UkT - X.UkN {
	  one e.map
    }

    // lock() to be transactionalised
	all e : X.LkT | some e1 : X'.EV {
      e.map = e1
      e1 in X'.R - X'.SCACQ // e1 is a lock-related read event
	  e1 not in ran[X'.rf] // e1 sees the lock is not taken
    }

	// unlock() to be transactionalised
    all e : X.UkT | no e.map

    // lock() to be kept as a normal lock
    all e : X.LkN | some disj e1, e2 : X'.EV {
      e.map = e1 + e2
      (e1 -> e2) in imm[X'.sb] & X'.atom // e1 and e2 form an RMW pair 
      e1 in X'.SCACQ & X'.R // e1 is a lock-related acquire read
      e2 in X'.W // e2 is a lock-related write
      e2 <: (X'.sb) in (dmb[none->none,X']) // BUGFIX: followed by a dmb
    }

    // unlock() to be kept as a normal unlock
    all e : X.UkN | some e1 : X'.EV {
 	  e.map = e1
      e1 in X'.SCREL & X'.W // eL is a lock-related release write
    }

  }   
}

pred consistent_critical_sections[X:Exec_Arm8L] {
  // no (po|com)-cycles among critical sections
  let com = X.rf + X.co + fr[none->none,X] |
  let scs = scst[X] + scsl[X] |
  is_acyclic[ scs . (X.sb + com - scs) . scs ]
}

pred p[X,X':Exec_Arm8L, map:E->E] {
  withoutinit[X]
  withoutinit[X']

 not {
    M/consistent[none->none, X]
    consistent_critical_sections[X]
  }

  //M/dead[none->none, X]

  M/consistent[none->none, X']

  apply_map[X,X',map]
/*
  some disj E0, E1, E2, E3, E4, E5, E6: E {
    hint_src[X,E0,E1,E2,E3,E4,E5,E6]
    hint_tgt[X',E0,E1,E2,E3,E4,E5,E6]
    map = (E0->E0) + (E1->E1) + (E3->E2) + (E3->E3) + (E4->E4) + (E5->E5) + (E6->E6)
  }*/
}

pred hint_src[X:Exec_Arm8L, E0,E1,E2,E3,E4,E5,E6:E] {
// E0:  LkT     || E3: LkN
// E1:  W x 1   || E4: R x 0
// E2:  UkT     ||     [data]
//              || E5: W x 2
//              || E6: UkN
    X.EV = E0+E1+E2+E3+E4+E5+E6
    X.R = E4
    X.W = E1+E5
    X.F = none
    X.LkT = E0
    X.UkT = E2
    X.LkN = E3
    X.UkN = E6
    X.SCACQ = none
    X.SCREL = none
    X.DMBLD = none
    X.DMBST = none
    X.co = (E1->E5)
    X.rf = none->none
    X.sloc = sq[E1+E4+E5]
    X.sthd = sq[E0+E1+E2] + sq[E3+E4+E5+E6]
    X.cd = none->none
    X.dd = E4->E5
    X.ad = none->none
    X.sb = ^((E0->E1)+(E1->E2)) + ^((E3->E4)+(E4->E5)+(E5->E6))
    X.atom = none->none
    X.stxn = none->none
}

pred hint_tgt[X:Exec_Arm8L, E0,E1,E2,E3,E4,E5,E6:E] {
//     txn {      || E2: R*[ACQ] lock 0   
// E0:   R lock 0 || E3: W* lock 1 
// E1:   W x 1    || E4: R x 0
//     }          ||     [data]
//                || E5: W x 2
//                || E6: W[REL] lock 2
    X.EV = E0+E1+E2+E3+E4+E5+E6
    X.R = E0+E2+E4
    X.W = E1+E3+E5+E6
    X.F = none
    X.LkT = none
    X.UkT = none
    X.LkN = none
    X.UkN = none
    X.SCACQ = E2
    X.SCREL = E6
    X.DMBLD = none
    X.DMBST = none
    X.co = (E1->E5)+(E3->E6)
    X.rf = none->none
    X.sloc = sq[E1+E4+E5] + sq[E0+E2+E3+E6]
    X.sthd = sq[E0+E1] + sq[E2+E3+E4+E5+E6]
    X.cd = none->none
    X.dd = E4->E5
    X.ad = none->none
    X.sb = (E0->E1) + ^((E2->E3)+(E3->E4)+(E4->E5)+(E5->E6))
    X.atom = (E2->E3)
    X.stxn = sq[E0+E1]
}

run p for 2 Exec, 8 E
