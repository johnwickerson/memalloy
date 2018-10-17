module exec_arm8[E]
open exec_arm7[E]

sig Exec_Arm8 extends Exec_Arm7 {
  SCREL : set E, // release events
  SCACQ : set E, // acquire events
}{

  // only reads can acquire
  SCACQ in R

  // only non-initial writes can release
  SCREL in W - IW
		
}

one sig rm_SCREL extends PTag {}
one sig rm_SCACQ extends PTag {}


fun SCREL[e:PTag->E, X:Exec_Arm8] : set E {
  X.SCREL - e[rm_EV] - e[rm_SCREL] }
fun SCACQ[e:PTag->E, X:Exec_Arm8] : set E {
  X.SCACQ - e[rm_EV] - e[rm_SCACQ] }
