module exec_x86L[E]
open exec_x86[E]

sig Exec_X86L extends Exec_X86 {
  scst : E->E, // same critical section, to be transactionalised
  scsl : E->E // same critical section, to be locked
}{

  // critical sections are intra-thread
  scst in sthd
  scsl in sthd

  // scst is a partial equivalence relation among a subset of
  // the non-initialisation events
  scst in (EV - IW) -> (EV - IW)
  symmetric[scst]
  transitive[scst]

  // scsl is a partial equivalence relation among a subset of
  // the non-initialisation events
  scsl in (EV - IW) -> (EV - IW)
  symmetric[scsl]
  transitive[scsl]

  // critical sections are contiguous in program order
  ((sb.sb & scst) . ~sb) & sb in scst
  ((sb.sb & scsl) . ~sb) & sb in scsl

  // no overlap between the two types of critical section
  no scst & scsl

  // the same location is never accessed by critical and 
  // non-critical events
  no (dom[scst + scsl] -> (EV - dom[scst + scsl])) & sloc
    
}
