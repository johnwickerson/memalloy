module exec_ptx[E]
open exec_H[E]

sig Exec_PTX extends Exec_H {
  scta, sgl : E->E, // same CTA, same global
  membar_cta,membar_gl,membar_sys:E->E
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

  is_fence_rel[membar_sys, sb]
  is_fence_rel[membar_gl, sb]
  is_fence_rel[membar_cta, sb]

  // A membar.sys implies a membar.gl, which implies a membar.cta
  membar_sys in membar_gl
  membar_gl in membar_cta
    
}

one sig rm_membar_sys extends PTag {}
one sig rm_membar_gl extends PTag {}
one sig rm_membar_cta extends PTag {}

fun scta[e:PTag->E, X:Exec_PTX] : E->E { rm_EV_rel[e, X.scta] }
fun sgl[e:PTag->E, X:Exec_PTX] : E->E { rm_EV_rel[e, X.sgl] }

fun membar_sys[e:PTag->E, X:Exec_PTX] : E->E {
  mk_fence_rel[e, rm_membar_sys+rm_membar_gl+rm_membar_cta,
    X.membar_sys, X.sb] }
fun membar_gl[e:PTag->E, X:Exec_PTX] : E->E {
  mk_fence_rel[e, rm_membar_gl + rm_membar_cta, X.membar_gl, X.sb] } 
fun membar_cta[e:PTag->E, X:Exec_PTX] : E->E {
  mk_fence_rel[e, rm_membar_cta, X.membar_cta, X.sb] }
