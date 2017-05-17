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
  WG = A
  DV in WG
  SY in DV
  FGA in DV + SY 
  REM in A 
}

one sig rm_WG extends PTag {}
one sig rm_DV extends PTag {}
one sig rm_SY extends PTag {}

fun WG [e:PTag->E, X:Exec_OpenCL] : set E {
  X.WG - e[rm_EV] - e[rm_WG] }
fun DV [e:PTag->E, X:Exec_OpenCL] : set E {
  X.DV - e[rm_EV] - e[rm_DV] - e[rm_WG] }
fun SY [e:PTag->E, X:Exec_OpenCL] : set E {
  X.SY - e[rm_EV] - e[rm_SY] - e[rm_DV] - e[rm_WG] }
fun FGA [e:PTag->E, X:Exec_OpenCL] : set E {
  X.FGA - e[rm_EV] }
fun G [e:PTag->E, X:Exec_OpenCL] : set E {
  X.G - e[rm_EV] }
fun L [e:PTag->E, X:Exec_OpenCL] : set E {
  X.L - e[rm_EV] }
fun REM [e:PTag->E, X:Exec_OpenCL] : set E {
  X.REM - e[rm_EV] }

fun swg [e:PTag->E, X:Exec_OpenCL] : E->E {
  (univ - e[rm_EV]) <: X.swg :> (univ - e[rm_EV]) }
fun sdv [e:PTag->E, X:Exec_OpenCL] : E->E {
  (univ - e[rm_EV]) <: X.sdv :> (univ - e[rm_EV]) }
