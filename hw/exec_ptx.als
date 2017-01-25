module exec_ptx[E]
open exec_H[E]

sig Exec_PTX extends Exec_H {
  membar_sys, membar_gl, membar_cta : set E, // memory barriers
  scta, sgl : E->E // same CTA, same global
}{
  
  // The membars are special kinds of fence
  membar_sys + membar_gl + membar_cta in F  

  // Can only be one kind of membar
  disj[membar_sys, membar_gl, membar_cta]

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

fun membar_sys[e:E, X:Exec_PTX] : set E { X.membar_sys - e }
fun membar_gl[e:E, X:Exec_PTX] : set E { X.membar_gl - e }
fun membar_cta[e:E, X:Exec_PTX] : set E { X.membar_cta - e }

fun scta[e:E, X:Exec_PTX] : E->E { X.scta - (univ -> e) - (e -> univ) }
fun sgl[e:E, X:Exec_PTX] : E->E { X.sgl - (univ -> e) - (e -> univ) }

run storebuffering_H for exactly 1 Exec, 4 E
