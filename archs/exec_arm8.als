module exec_arm8[E]
open exec_arm7[E]

sig Exec_Arm8 extends Exec_Arm7 {
  SCREL : set E, // release events
  SCACQ : set E  // acquire events
}{

  // only reads can acquire
  SCACQ in R

  // only non-initial writes can release
  SCREL in W - IW
    
}

fun SCREL[e:E, X:Exec_Arm8] : set E { X.SCREL - e }
fun SCACQ[e:E, X:Exec_Arm8] : set E { X.SCACQ - e }
