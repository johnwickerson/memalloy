module exec_ptx[E]
open exec_H[E]

sig Exec_PTX extends Exec_H {
  scta, sgl : E->E, // same CTA, same global
  MEMBAR_CTA, MEMBAR_GL, MEMBAR_SYS : set E // memory barriers
}{

  // scta and sgl are equivalence relations among all events
  is_equivalence[scta, EV]
  is_equivalence[sgl, EV]

  // Same thread implies same cta
  sthd in scta

  // Same CTA implies same global
  scta in sgl

  // There are no RMW events, instead we use the atom relation
  // to link the R and the W events
  no (R&W)

  // Atom relation not done yet
  no atom
    
  // A membar.sys implies a membar.gl, which implies a membar.cta
  MEMBAR_SYS in MEMBAR_GL
  MEMBAR_GL in MEMBAR_CTA
  MEMBAR_CTA in F
    
}

fun scta[e:PTag->E, X:Exec_PTX] : E->E {
  (univ - e[rm_EV]) <: X.scta :> (univ - e[rm_EV]) }
fun sgl[e:PTag->E, X:Exec_PTX] : E->E {
  (univ - e[rm_EV]) <: X.sgl :> (univ - e[rm_EV]) }
          
fun MEMBAR_SYS[e:PTag->E, X:Exec_PTX] : set E { X.MEMBAR_SYS - e[rm_EV] }
fun MEMBAR_GL[e:PTag->E, X:Exec_PTX] : set E { X.MEMBAR_GL - e[rm_EV] }
fun MEMBAR_CTA[e:PTag->E, X:Exec_PTX] : set E { X.MEMBAR_CTA - e[rm_EV] }

fun membar_sys[e:PTag->E, X:Exec_PTX] : E->E { addsb[e,X,MEMBAR_SYS[e,X]] }
fun membar_gl[e:PTag->E, X:Exec_PTX] : E->E { addsb[e,X,MEMBAR_GL[e,X]] }
fun membar_cta[e:PTag->E, X:Exec_PTX] : E->E { addsb[e,X,MEMBAR_CTA[e,X]] }
