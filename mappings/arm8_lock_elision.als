open ../archs/exec_arm8L[E]
open ../models/aarch64_txn[E] as M

module arm8_lock_elision[E]

pred apply_map[X,X':Exec_Arm8L, map:E->E] { 


  map in (X.EV one->some X'.EV)

  // no critical sections remain
  no X'.(scst+scsl)

  // don't worry about failing transactions for now
  no X.ftxn + X'.ftxn

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

    X.R     = (X'.R     - L).~map
    X.W     = (X'.W     - L).~map
    X.F     = (X'.F     - L).~map
    X.DMB   = (X'.DMB   - L).~map
    X.DMBST = (X'.DMBST - L).~map
    X.DMBLD = (X'.DMBLD - L).~map
    X.ISB   = (X'.ISB   - L).~map
    X.SCACQ = (X'.SCACQ - L).~map
    X.SCREL = (X'.SCREL - L).~map 

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
          eLR in L & X'.SCACQ & X'.R // eLR is a lock-related acquire read
          eLW in L & X'.W // eLW is a lock-related write
        }
      )
      e not in dom[X.sb & X.scsl] => (
        // e is last in its critical section
        some disj e', eL : X'.EV {
          e.map = e' + eL // e maps to two events, e' and eL
          e' not in L // e' is not a lock-related event
          (e' -> eL) in imm[X'.sb] // eL follows e' in program order
          eL in L & X'.SCREL & X'.W // eL is a lock-related release write 
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

pred consistent_critical_sections[X:Exec_Arm8L] {
  // no (po|com)-cycles among critical sections
  let com = X.rf + X.co + fr[none->none,X] |
  let scs = X.scst + X.scsl |
  is_acyclic[ scs . (X.sb + com - scs) . scs ]
}

pred p[X,X':Exec_Arm8L, map:E->E] {
  withoutinit[X]
  withoutinit[X']

  not {
    M/consistent[none->none, X]
    consistent_critical_sections[X]
  }

  M/dead[none->none, X]

  M/consistent[none->none, X']

  apply_map[X,X',map]

  //hint_src[X]
  //hint_tgt[X']
}

pred hint_src[X:Exec_Arm8L] {
//     lock[t] { ||     lock {
// E0:   W x 1   || E1:   R x 0
//     }         ||       [data]
//               || E2:   W x 2
//               ||     }
  some disj E0, E1, E2 : E {
    X.EV = E0+E1+E2
    X.R = E1
    X.W = E0+E2
    X.F = none
    X.SCACQ = none
    X.SCREL = none
    X.DMBLD = none
    X.DMBST = none
    X.DMB = none
    X.co = (E0->E2)
    X.rf = none->none
    X.sloc = sq[E0+E1+E2]
    X.sthd = sq[E0] + sq[E1+E2]
    X.cd = none->none
    X.dd = E1->E2
    X.ad = none->none
    X.sb = (E1->E2)
    X.atom = none->none
    X.scst = sq[E0]
    X.scsl = sq[E1+E2]
    X.ftxn = none->none
    X.stxn = none->none
  }
}

pred hint_tgt[X:Exec_Arm8L] {
//     txn {      || E5: R*[ACQ] lock 0   
// E3:   R lock 0 || E6: W* lock 1 
// E4:   W x 1    || E7: R x 0
//     }          ||     [data]
//                || E8: W x 2
//                || E9: W[REL] lock 2

  some disj E3, E4, E5, E6, E7, E8, E9 : E {
    X.EV = E3+E4+E5+E6+E7+E8+E9
    X.R = E3+E5+E7
    X.W = E4+E6+E8+E9
    X.F = none
    X.SCACQ = E5
    X.SCREL = E9
    X.DMBLD = none
    X.DMBST = none
    X.DMB = none
    X.co = (E4->E8)+(E6->E9)
    X.rf = none->none
    X.sloc = sq[E4+E7+E8] + sq[E3+E5+E6+E9]
    X.sthd = sq[E3+E4] + sq[E5+E6+E7+E8+E9]
    X.cd = none->none
    X.dd = E7->E8
    X.ad = none->none
    X.sb = (E3->E4) + ^((E5->E6)+(E6->E7)+(E7->E8)+(E8->E9))
    X.atom = (E5->E6)
    X.scst = none->none
    X.scsl = none->none
    X.ftxn = none->none
    X.stxn = sq[E3+E4]
  }
}

run p for 2 Exec, 6 E
