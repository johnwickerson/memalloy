module exec_arm8[E]
open exec_arm7[E]

sig Exec_Arm8 extends Exec_Arm7 {
  screl : set E, // release events
  scacq : set E  // acquire events
}{

  // only atomic reads can acquire
  scacq in R

  // only atomic writes can release
  screl in W - IW
    
}

fun screl[e:E, X:Exec_Arm8] : set E { X.screl - e }
fun scacq[e:E, X:Exec_Arm8] : set E { X.scacq - e }
