module checkdead_x86[E]
open preexecs_x86[E] as tl
open x86tso[E] as M1

pred p1 [p : Prog,
         x, x' : Exec,
         enabled, enabled' : set GInst,
         post, post' : Addr -> Val] 
{

  // prefer to avoid RMWs for the time being
  //no_RMWs[x]
  no x.atom

  // We generate an inconsistent execution x
  not (M1/consistent[x])

  // We generate the "canonical" program p from x
  cand_exec_x86[p,x,enabled,post]
  is_canonical_prog[p,x.rf,enabled,post]
  
  // We obtain another execution x' from the program
  cand_exec_x86[p,x',enabled',post']

  // What we *don't* want:
  M1/consistent[x'] 
  not (M1/racefree[x']) or post = post'
  
  // The following condition on x is designed 
  // to rule out unwanted executions like x'
  M1/dead[x]
}

run p1 for 4 Int, 
1 Prog, 8 Cmd, 3 Val, 3 Loc, 4 Reg, 
2 Exec, 4 E 
expect 0
// No soln, 5 mins, babillion, plingeling

run p1 for 4 Int, 
1 Prog, 10 Cmd, 3 Val, 3 Loc, 4 Reg, 
2 Exec, 5 E 
expect 0
// Not run yet

run p1 for 4 Int, 
1 Prog, 12 Cmd, 3 Val, 3 Loc, 6 Reg, 
2 Exec, 6 E 
expect 0
// Cancelled (3 days, plingeling @ benjamin)



