module exec_OpenCL[E]
open ../archs/exec_C[E]

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

  // swg and sdv are equivalence relations among non-initial events
  is_equivalence[swg, ev - IW]
  is_equivalence[sdv, ev - IW]

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

fun wg [e:E, X:Exec_OpenCL] : set E { X.wg - e }
fun dv [e:E, X:Exec_OpenCL] : set E { X.dv - e }
fun sy [e:E, X:Exec_OpenCL] : set E { X.sy - e }
fun fga [e:E, X:Exec_OpenCL] : set E { X.fga - e }
fun G [e:E, X:Exec_OpenCL] : set E { X.G - e }
fun L [e:E, X:Exec_OpenCL] : set E { X.L - e }
fun entry_fence [e:E, X:Exec_OpenCL] : set E { X.entry_fence - e }
fun exit_fence [e:E, X:Exec_OpenCL] : set E { X.exit_fence - e }
fun rem [e:E, X:Exec_OpenCL] : set E { X.rem - e }
fun swg [e:E, X:Exec_OpenCL] : E -> E { X.swg - (e -> univ) - (univ -> e) }
fun sdv [e:E, X:Exec_OpenCL] : E -> E { X.sdv - (e -> univ) - (univ -> e) }
fun sbar [e:E, X:Exec_OpenCL] : E -> E { X.sbar - (e -> univ) - (univ -> e) }
