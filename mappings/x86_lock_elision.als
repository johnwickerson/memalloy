open ../archs/exec_x86L[E]
open ../models/x86tso_txn[E] as M

module x86_lock_elision[E]

pred apply_map[X,X':Exec_X86L, map:E->E] { 


  map in (X.EV one->some X'.EV)

  // no critical sections remain
  no X'.(scst+scsl)

  // suppose we don't have transactions in the source program
  no X.stxn

  // events in the same critical section (where the critical
  // section is one that is to be transactionalised), map to events 
  // in the same transaction.
  X.scst in map.(X'.stxn).~map

  some L : set E { 

    X.stxn in map.(X'.stxn).~map

    // Mildly interesting observation: transactional memory models
    // are not monotonic w.r.t. adding sequencing edges

    X.sthd = map.((X'.EV - L) <: (X'.sthd) :> (X'.EV - L)).~map
    X.sb =   map.((X'.EV - L) <: (X'.sb)   :> (X'.EV - L)).~map
    X.ad =   map.((X'.EV - L) <: (X'.ad)   :> (X'.EV - L)).~map
    X.cd =   map.((X'.EV - L) <: (X'.cd)   :> (X'.EV - L)).~map
    X.dd =   map.((X'.EV - L) <: (X'.dd)   :> (X'.EV - L)).~map
    X.atom = map.((X'.EV - L) <: (X'.atom) :> (X'.EV - L)).~map
    X.rf =   map.((X'.EV - L) <: (X'.rf)   :> (X'.EV - L)).~map
    X.co =   map.((X'.EV - L) <: (X'.co)   :> (X'.EV - L)).~map
 
    X.R      = (X'.R      - L).~map
    X.W      = (X'.W      - L).~map
    X.F      = (X'.F      - L).~map
    X.MFENCE = (X'.MFENCE - L).~map

    // the set of lock-related events introduced by the mapping
    L in X'.EV 

    // lock-related events in X' are on the same location
    X'.sloc = (L -> L) + ((X'.EV - L) <: ~map.(X.sloc).map :> (X'.EV - L))

    all e : dom[X.scst] {
      // e is in a critical section to be transactionalised 
      e not in ran[X.sb & X.scst] => (
        // e is first in its critical section 
        some disj eL, e' : X'.EV {
          e.map = eL + e' // e maps to two events, eL and e'
          e' not in L // e' is not a lock-related event
          (eL -> e') in imm[X'.sb] & X'.stxn // eL is first in the transaction
          eL in L & X'.R // eL is a lock-related read event
          eL not in ran[X'.rf] // eL sees the lock is not taken
        }
      )
      e in ran[X.sb & X.scst] => {
        // e is not first in its critical section
        one e.map // e maps to a single event
        e.map not in L // e does not map to a lock-related event
      }
    }

    all e : dom[X.scsl] {
      e not in ran[X.sb & X.scsl] => (
        // e is first in its critical section
        some disj eLR, eLW, e' : X'.EV {
          e.map = eLR + eLW + e' // e maps to three events, eLR, eLW, and e'
          e' not in L // e' is not a lock-related event
          (eLR -> eLW) in imm[X'.sb] & X'.atom // eLR and eLW form an RMW pair
          (eLW -> e') in imm[X'.sb] // eLW precedes e' in program order
          eLR in L & X'.R // eLR is a lock-related acquire read
          eLW in L & X'.W // eLW is a lock-related write
        }
      )
      e not in dom[X.sb & X.scsl] => (
        // e is last in its critical section
        some disj e', eL : X'.EV {
          e.map = e' + eL // e maps to two events, e' and eL
          e' not in L // e' is not a lock-related event
          (e' -> eL) in imm[X'.sb] // eL follows e' in program order
          eL in L & X'.W // eL is a lock-related release write 
        }
      ) 
      e in dom[X.sb & X.scsl] & ran[X.sb & X.scsl] => {
        // e is neither first nor last in its critical section
        one e.map // e maps to a single event
        e.map not in L // e does not map to a lock-related event
      }
    }

    all e : X.EV - dom[X.scsl] - dom[X.scst] {
      // e is not in a critical section
      one e.map // e maps to a single event
      e.map not in L // e does not map to a lock-related event
    }
  
  }   
   
}

pred consistent_critical_sections[X:Exec_X86L] {
  // no (po|com)-cycles among critical sections
  let com = X.rf + X.co + fr[none->none,X] |
  let scs = X.scst + X.scsl |
  is_acyclic[ scs . (X.sb + com - scs) . scs ]
}

pred p[X,X':Exec_X86L, map:E->E] {
  withoutinit[X]
  withoutinit[X']

  not {
    M/consistent[none->none, X]
    consistent_critical_sections[X]
  }

  M/dead[none->none, X]

  M/consistent[none->none, X']

  apply_map[X,X',map]
}

run p for 2 Exec, 7 E // started on benjamin at 0943 on fri 3 nov
