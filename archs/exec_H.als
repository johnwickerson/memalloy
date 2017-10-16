module exec_H[E]
open exec[E]

sig Exec_H extends Exec {
  atom : E -> E // atomicity relation
}{

  // the atom relation relates a consecutively-sequenced read/write pair
  atom in (R->W) & imm[sb] & sloc
    
  // there are no single-event RMWs
  no (R&W)

  // control dependencies are defined differently in assembly
  cd.sb in cd

  // the atom relation relates either
  // - two events in the same transaction or
  // - two events that are non-transactional
  atom in stxn + ((EV - dom[stxn]) -> (EV - dom[stxn]))
    
}

one sig rm_atom extends PTag {}

fun atom[e:PTag->E, X:Exec_H] : E->E {
  (univ - e[rm_EV] - e[rm_atom]) <: X.atom :> (univ - e[rm_EV]) }
