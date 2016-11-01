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

run storebuffering_H for exactly 1 Exec, 4 E
