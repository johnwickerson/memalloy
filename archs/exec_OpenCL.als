module exec_OpenCL[E]
open exec_C[E]

sig Exec_OpenCL extends Exec_C {
  DV, SY : set E, // device- and system-scoped events (NB: workgroup-scoped events are simply atomic events (A) so do not need their own category)
  FGA : set E, // events accessing fine-grained atomic buffers
  G, L : set E, // events that access global/local locations
  swg, sdv : E -> E, // same-workgroup, same-device equivalences
/////////////////////////////////////////////////////////////
  REM : set E // remote-scoped events (for RSP extension)
}{
  
  // global and local memory accesses comprise zero or more sloc-classes
  (G - F) . sloc = (G - F)
  (L - F) . sloc = (L - F)

  // FGA accesses comprise zero or more sloc-classes		
  FGA . sloc = FGA

  // FGA buffers are in global memory		
  FGA in G
		
  // every event is either global or local
  EV in G + L

  // only fences can be both global and local.
  (G & L) in F

  // swg and sdv are equivalence relations among non-initial events
  is_equivalence[swg, EV - IW]
  is_equivalence[sdv, EV - IW]

  // Same thread implies same workgroup
  sthd in swg

  // Same workgroup implies same device
  swg in sdv

  // Scopes are only applied to atomic events, and wider
  // scopes include all narrower scopes
  DV in A
  SY in DV
  FGA in DV 
  REM in A 
}

one sig rm_DV extends PTag {}
one sig rm_SY extends PTag {}
one sig rm_FGA extends PTag {}
one sig rm_REM extends PTag {}

fun DV [e:PTag->E, X:Exec_OpenCL] : set E {
  X.DV - e[rm_EV] - e[rm_DV] - e[rm_A] }
fun SY [e:PTag->E, X:Exec_OpenCL] : set E {
  X.SY - e[rm_EV] - e[rm_SY] - e[rm_DV] - e[rm_A] }
fun FGA [e:PTag->E, X:Exec_OpenCL] : set E {
  X.FGA - e[rm_EV] - e[rm_FGA] - e[rm_DV] - e[rm_A] }
fun G [e:PTag->E, X:Exec_OpenCL] : set E {
  X.G - e[rm_EV] }
fun L [e:PTag->E, X:Exec_OpenCL] : set E {
  X.L - e[rm_EV] }
fun REM [e:PTag->E, X:Exec_OpenCL] : set E {
  X.REM - e[rm_EV] - e[rm_A] }

fun swg [e:PTag->E, X:Exec_OpenCL] : E->E {
  (univ - e[rm_EV]) <: X.swg :> (univ - e[rm_EV]) }
fun sdv [e:PTag->E, X:Exec_OpenCL] : E->E {
  (univ - e[rm_EV]) <: X.sdv :> (univ - e[rm_EV]) }
