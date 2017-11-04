module exec_x86L[E]
open exec_x86[E]

fun scst[X:Exec_X86L] : E->E {
  let Uk = X.UkT + X.UkN |
  let sb_noUk = (X.EV - Uk) <: imm[X.sb] |
  ~*sb_noUk . (stor[X.LkT]) . *sb_noUk
}

fun scsl[X:Exec_X86L] : E->E {
  let Uk = X.UkT + X.UkN |
  let sb_noUk = (X.EV - Uk) <: imm[X.sb] |
  ~*sb_noUk . (stor[X.LkN]) . *sb_noUk
}

sig Exec_X86L extends Exec_X86 {
  LkT : set E, // lock to be transactionalised
  UkT : set E, // unlock to be transactionalised
  LkN : set E, // normal lock
  UkN : set E  // normal unlock
}{
  
  let Lk = LkT + LkN |
  let Uk = UkT + UkN {

    // Lk/Uk events are disjoint from each other and all other events
    disj[LkT, UkT, LkN, UkN, R+W+F]

    // Lk/Uk pairs must be well-nested.
    LkT <: sb :> (Lk + UkN) in (sb :> UkT) . sb
    LkN <: sb :> (Lk + UkT) in (sb :> UkN) . sb
    Uk <: sb :> Uk in (sb :> Lk) . sb

    // Every lock must be followed by an unlock
    Lk in sb.Uk

    // the same location is never accessed by critical and 
    // non-critical events
    no (dom[scst[this] + scsl[this]] -> (EV - dom[scst[this] + scsl[this]])) & sloc
    
  }

}
