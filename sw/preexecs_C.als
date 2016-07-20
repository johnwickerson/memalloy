module preexecs_C[E]
open ../preexecs[E]
open prog_C
open exec_C[E]

pred apply_pi_C[pi : Inst -> E, p : Prog_C, x : Exec_C, 
              enabled : set Cmd, post : Addr -> Val] 
{
  apply_pi[pi,p,x,enabled,post]

  all i : enabled & Inst {

    // atomic operations
    i.pi in x.A iff i in p.insts_atomic

    // memory order: acquire
    i.pi in x.acq iff (i in p.insts_acq and not failed_cas[i,pi,x.rf])
 
    // memory order: release
    i.pi in x.rel iff (i in p.insts_rel and not failed_cas[i,pi,x.rf])

    // memory order: sc
    i.pi in x.sc iff (i in p.insts_sc and not failed_cas[i,pi,x.rf])
  }
}

pred cand_exec_C[p : Prog_C, x : Exec_C, enabled : set Cmd, 
               post : Addr -> Val] 
{
  some pi : Inst -> E | apply_pi_C[pi,p,x,enabled,post]
}
