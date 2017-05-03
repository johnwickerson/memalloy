module exec_ptx[E]
open exec_H[E]

sig Exec_PTX extends Exec_H {
  scta, sgl : E->E, // same CTA, same global
  MEMBAR_CTA, MEMBAR_GL, MEMBAR_SYS : set E // memory barriers
}{

  // scta and sgl are equivalence relations among all events
  is_equivalence[scta, ev]
  is_equivalence[sgl, ev]

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

fun scta[e:E, X:Exec_PTX] : E->E { X.scta - (univ -> e) - (e -> univ) }
fun sgl[e:E, X:Exec_PTX] : E->E { X.sgl - (univ -> e) - (e -> univ) }
fun MEMBAR_SYS[e:E, X:Exec_PTX] : set E { X.MEMBAR_SYS - e }
fun MEMBAR_GL[e:E, X:Exec_PTX] : set E { X.MEMBAR_GL - e }
fun MEMBAR_CTA[e:E, X:Exec_PTX] : set E { X.MEMBAR_CTA - e }

fun membar_sys[e:E, X:Exec_PTX] : E->E { addsb[e,X,MEMBAR_SYS[e,X]] }
fun membar_gl[e:E, X:Exec_PTX] : E->E { addsb[e,X,MEMBAR_GL[e,X]] }
fun membar_cta[e:E, X:Exec_PTX] : E->E { addsb[e,X,MEMBAR_CTA[e,X]] }
