module exec_OpenCL[E]
open exec_C[E]

sig Exec_OpenCL extends Exec_C {
  WG, DV, SY : set E, // workgroup-, device- and system-scoped events
  FGA : set E, // events accessing fine-grained atomic buffers
  G, L : set E, // events that access global/local locations
  swg, sdv : E -> E, // same-workgroup, same-device equivalences
/////////////////////////////////////////////////////////////
  REM : set E // remote-scoped events (for RSP extension)
}{
  
  // global and local memory accesses comprise zero or more sloc-classes
  (G - F) . sloc = (G - F)
  (L - F) . sloc = (L - F)

  // every event is either global or local
  EV in G + L

  // only fences can be both global and local.
  no ((G & L) - F)

  // swg and sdv are equivalence relations among non-initial events
  is_equivalence[swg, EV - IW]
  is_equivalence[sdv, EV - IW]

  // Same thread implies same workgroup
  sthd in swg

  // Same workgroup implies same device
  swg in sdv

  // Scopes are only applied to atomic events, and wider
  // scopes include all narrower scopes
  WG = ATO
  DV in WG
  SY in DV
  FGA in DV + SY 
  REM in A 
}

fun WG [e:E, X:Exec_OpenCL] : set E { X.WG - e }
fun DV [e:E, X:Exec_OpenCL] : set E { X.DV - e }
fun SY [e:E, X:Exec_OpenCL] : set E { X.SY - e }
fun FGA [e:E, X:Exec_OpenCL] : set E { X.FGA - e }
fun G [e:E, X:Exec_OpenCL] : set E { X.G - e }
fun L [e:E, X:Exec_OpenCL] : set E { X.L - e }
fun REM [e:E, X:Exec_OpenCL] : set E { X.REM - e }

fun swg [e:E, X:Exec_OpenCL] : E->E { X.swg - (e -> univ) - (univ -> e) }
fun sdv [e:E, X:Exec_OpenCL] : E->E { X.sdv - (e -> univ) - (univ -> e) }
