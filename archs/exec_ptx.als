module exec_ptx[E]
open exec_H[E]

sig Exec_PTX extends Exec_H {
  scta, sgl : E->E // same CTA, same global
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
    
}

pred wf_Exec_PTX[X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] {
    
  wf_Exec_H[X,ad,cd,dd]

  is_fence_rel[membar_sys, X.sb]
  is_fence_rel[membar_gl, X.sb]
  is_fence_rel[membar_cta, X.sb]

  // A membar.sys implies a membar.gl, which implies a membar.cta
  membar_sys in membar_gl
  membar_gl in membar_cta
    
}

fun scta[e:E, X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] : E->E { X.scta - (univ -> e) - (e -> univ) }
fun sgl[e:E, X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] : E->E { X.sgl - (univ -> e) - (e -> univ) }
fun membar_sys[e:E, X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] : set E { membar_sys - (univ -> e) - (e -> univ) }
fun membar_gl[e:E, X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] : set E { membar_gl - (univ -> e) - (e -> univ) }
fun membar_cta[e:E, X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] : set E { membar_cta - (univ -> e) - (e -> univ) }

// Synonyms
fun membarsys[e:E, X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] : set E { membar_sys[e,X,ad,cd,dd,membar_cta,membar_gl,membar_sys] }
fun membargl[e:E, X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] : set E { membar_gl[e,X,ad,cd,dd,membar_cta,membar_gl,membar_sys] }
fun membarcta[e:E, X:Exec_PTX, ad,cd,dd,membar_cta,membar_gl,membar_sys:E->E] : set E { membar_cta[e,X,ad,cd,dd,membar_cta,membar_gl,membar_sys] }
