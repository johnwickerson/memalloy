module exec_ppcL[E]
open exec_ppc[E]

sig Exec_PPCL extends Exec_PPC {
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
