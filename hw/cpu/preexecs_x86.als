module preexecs_x86[E]
open ../../preexecs[E]
open prog_x86
open exec_x86[E]

pred apply_pi_x86[pi : Inst -> E, p : Prog_x86, x : Exec_X86, 
              enabled : set Cmd, post : Addr -> Val] 
{
  apply_pi[pi,p,x,enabled,post]

  all a : enabled & Inst {

    // locked operations
    a.pi in x.locked iff a in p.insts_locked

  }
}

pred cand_exec_x86[p : Prog_x86, x : Exec_X86, enabled : set Cmd, 
               post : Addr -> Val] 
{
  some pi : Inst -> E | apply_pi_x86[pi,p,x,enabled,post]
}
