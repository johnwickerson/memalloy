module exec_arm8[E]
open exec_arm7[E]

sig Exec_Arm8 extends Exec_Arm7 {
  A : set E, // atomic events
  screl : set E, // release events
  scacq : set E  // acquire events
}{

  //only reads and writes can be atomic
  A in (R + W)

  // only atomic reads can acquire
  scacq in (R & A)

  // only atomic writes can release
  screl in (W & A)
    
}

fun A[e:E, X:Exec_Arm8] : set E { X.A - e }
fun screl[e:E, X:Exec_Arm8] : set E { X.screl - e }
fun scacq[e:E, X:Exec_Arm8] : set E { X.scacq - e }
