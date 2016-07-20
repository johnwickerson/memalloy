module checkdead_C[E]
open preexecs_C[E] as tl
open c11_simp[E] as M1

pred checkdead [p : Prog_C,
         x, x' : Exec_C, 
         enabled, enabled' : set Inst,
         post, post' : Addr -> Val] 
{

  // prefer to avoid RMWs for the time being
  //no_RMWs[x]

  // We generate an inconsistent execution x
  not (M1/consistent[x])

  // We generate the "canonical" program p from x
  cand_exec_C[p,x,enabled,post]
  is_canonical_prog[p,x.rf,enabled,post]
  
  // We obtain another execution x' from the program
  cand_exec_C[p,x',enabled',post']

  // What we *don't* want:
  M1/consistent[x'] 
  not (M1/racefree[x']) or post = post'
  
  // The following conditions on x is designed 
  // to rule out unwanted executions like x'
  M1/dead[x]
}

run checkdead for 4 Int, 
1 Prog, 8 Cmd, 3 Val, 3 Loc, 4 Reg, 
2 Exec, 2 E 
expect 0
// no soln (1s, babillion, glucose) 

run checkdead for 4 Int, 
1 Prog, 10 Cmd, 3 Val, 3 Loc, 4 Reg, 
2 Exec, 5 E 
expect 0
// no soln (1 min, babillion, glucose)

run checkdead for 4 Int, 
1 Prog, 12 Cmd, 3 Val, 3 Loc, 6 Reg, 
2 Exec, 6 E 
expect 0
// no soln (6 min, babillion, glucose)

run checkdead for 4 Int, 
1 Prog, 14 Cmd, 3 Val, 3 Loc, 8 Reg, 
2 Exec, 7 E 
expect 0
// no soln (12 min, babillion, glucose)


run checkdead for 4 Int, 
1 Prog, 15 Cmd, 3 Val, 3 Loc, 9 Reg, 
2 Exec, 8 E expect 0
// no soln (72 min, benjamin, glucose)

