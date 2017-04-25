module exec_ptx[E]
open exec_H[E]

sig Exec_PTX extends Exec_H {
  scta, sgl : E->E, // same CTA, same global
  membar_cta,membar_gl,membar_sys : set E // memory barriers
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
  membar_sys in membar_gl
  membar_gl in membar_cta
  membar_cta in F
    
}

fun scta[e:E, X:Exec_PTX] : E->E { X.scta - (univ -> e) - (e -> univ) }
fun sgl[e:E, X:Exec_PTX] : E->E { X.sgl - (univ -> e) - (e -> univ) }
fun membar_sys[e:E, X:Exec_PTX] : set E { X.membar_sys - e }
fun membar_gl[e:E, X:Exec_PTX] : set E { X.membar_gl - e }
fun membar_cta[e:E, X:Exec_PTX] : set E { X.membar_cta - e }

// Synonyms
fun membarsys[e:E, X:Exec_PTX] : set E { membar_sys[e,X] }
fun membargl[e:E, X:Exec_PTX] : set E { membar_gl[e,X] }
fun membarcta[e:E, X:Exec_PTX] : set E { membar_cta[e,X] }
