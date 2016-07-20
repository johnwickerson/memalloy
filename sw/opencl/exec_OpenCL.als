module exec_OpenCL[E]
open ../exec_C[E]

sig Exec_OpenCL extends Exec_C {
  wg, dv, sy : set E, // workgroup-, device- and system-scoped events
  fga : set E, // events accessing fine-grained atomic buffers
  G, L : set E, // events that access global/local locations
  swg, sdv : E -> E, // same-workgroup, same-device equivalences
  entry_fence, exit_fence : set E, // barrier entry/exit fences
  sbar : E -> E, // same-barrier equivalence
/////////////////////////////////////////////////////////////
  rem : set E, // remote-scoped events (for RSP extension)
}{
  
  // global and local memory accesses comprise zero or more sloc-classes
  (G - F) . sloc = (G - F)
  (L - F) . sloc = (L - F)

  // every event is either global or local
  ev in G + L

  // only fences can be both global and local.
  no ((G & L) - F)

  // swg and sdv are equivalence relations among all events
  is_equivalence[swg, ev]
  is_equivalence[sdv, ev]

  // Same thread implies same workgroup
  sthd in swg

  // Same workgroup implies same device
  swg in sdv

  // Scopes are only applied to atomic events, and wider
  // scopes include all narrower scopes
  wg = A
  dv in wg
  sy in dv
  fga in dv + sy 
  rem in A 

  // We do not handle barriers at the moment
  no sbar
  no entry_fence
  no exit_fence
}
