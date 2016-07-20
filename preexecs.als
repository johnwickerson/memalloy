module preexecs[E]
open prog
open exec[E]

pred failed_cas[i: Inst, pi : Inst -> E, rf : E -> E] {
  i in Cas and i.rval != i.pi.~rf.~pi.wval
}

pred successful_cas[i: Inst, pi : Inst -> E, rf : E -> E] {
  i in Cas and i.rval = i.pi.~rf.~pi.wval
}

pred apply_pi
  [pi : Inst -> E, p : Prog, x : Exec, enabled : set Cmd, post : Addr -> Val] 
{

  pi in (enabled & Inst) one->one x.ev
  //x.ev = E

  check_enabled_actions[enabled, p, post]

  // the only registers are those that are loaded/exchanged into
  //Reg in (Load + Cas) <: reg

  all i : enabled & Inst {

    // read => load or failed cas
    i.pi in x.(R-W) iff 
      (i in Load or failed_cas[i,pi,x.rf])
  
    // write => store
    i.pi in x.(W-R) iff 
      i in Store
   
    // RMW => successful cas
    i.pi in x.(R&W) iff 
      successful_cas[i,pi,x.rf]
 
    // fence => fence
    i.pi in x.F iff i in Fence

    // atomic locations
    one i.loc implies (i.loc in NALoc iff i.pi in x.naL)

  }

  // same thread
  all i, j : enabled & Inst |
    (i -> j) in same_thread[p] iff (i.pi -> j.pi) in x.sthd

  
  // same location
  all i, j : enabled & Inst |
    (one i.loc && one j.loc && i.loc = j.loc) 
    iff
    (i.pi -> j.pi) in x.sloc
 
  // control dependencies 
  all i, j : enabled & Inst | 
    (some g : If & enabled {
       (i -> g) in sequenced[p]
       i in Load + Cas
       i.reg = g.reg 
       j in g.^(child[p] :> enabled)
    })
    iff
    (i.pi -> j.pi) in x.cd   

  // address dependencies
  all i, j : enabled & Inst |
    (i in Load + Cas && i.reg in j.fake_loc_deps)
    iff
    (i.pi -> j.pi) in x.ad

  // data dependencies
  all i, j : enabled & Inst |
    (i in Load + Cas && i.reg in j.fake_wval_deps)
    iff
    (i.pi -> j.pi) in x.dd

  // sequenced before
  all cmd : enabled & Seq |
    all i : cmd.(p.fst).*(child[p]) & enabled & Inst |
      all j : cmd.(p.snd).*(child[p]) & enabled & Inst |
        (i.pi -> j.pi) in x.sb
  all cmd : enabled & Unseq |
    all i : cmd.(p.fst).*(child[p]) & enabled & Inst |
      all j : cmd.(p.snd).*(child[p]) & enabled & Inst |
        (i.pi -> j.pi) not in x.sb + ~(x.sb)

  // reads-from
  all j: enabled & (Load + Cas) |
    (all i: enabled & (Store + Cas) | 
      (i.loc = j.loc && i.wval = post[j.reg]) iff
        (i.pi -> j.pi) in x.rf) and
    ((post[j.reg] = Zero) iff no j.pi.~(x.rf))

  // final writes
  all l : Loc | (
    // "i" is the last write to location "l" so post[l] takes 
    // the value that "i" writes
    some i : enabled & (Store + Cas) { 
      i.loc = l
      co_terminator[i.pi, x]
      post[l] = i.wval
    }
  ) or (
    // location "l" is never written, so post[l] remains 0
    (all a : enabled & (Store + Cas) | a.loc != l) and post[l] = Zero
  )

} 

pred co_terminator[e:E, x:Exec] {
  all e' : x.W | 
    (e -> e') in x.sloc - iden implies (e -> e') not in x.co
}

pred is_canonical_prog
  [p : Prog, rf : E->E, enabled : set Cmd, post : Addr -> Val] 
{
  // every command is enabled
  p.cm in enabled

  // every CAS succeeds
  all i : p.cm & Cas | post[i.reg] = i.rval
}

pred cand_exec
  [p : Prog, x : Exec, enabled : set Cmd, post : Addr -> Val] 
{
  some pi : Inst -> E | apply_pi[pi,p,x,enabled,post]
}

pred cand_exec' [p : Prog, x : Exec, post : Addr -> Val] {
  some enabled : set Cmd | cand_exec[p,x,enabled,post]
}

pred p1 [p : Prog, x : Exec, enabled : set Cmd, 
         post : Addr -> Val] 
{
  cand_exec[p,x,enabled,post]
  is_canonical_prog[p,x.rf,enabled,post]
  simpleexec[x]
  simplelit[p,post]
}

pred p2 [p : Prog, x : Exec, enabled : set Cmd, 
         post : Addr -> Val] 
{
  cand_exec[p,x,enabled,post]
  is_canonical_prog[p,x.rf,enabled,post]
  messagepassing[x]
}

pred simplelit[p:Prog, post:Addr->Val] {
  /*
  store(x,1); //i1
  r=load(x); //i2
  */
  some v1 : Val, x : ALoc, r : Reg, c : Seq, i1 : Store, i2 : Load {
    Loc = x
    Reg = r
    p.top = c
    p.fst = (c -> i1)
    p.snd = (c -> i2)
    i1.wval = v1
    i1.loc = x
    i2.loc = x
    i2.reg = r
    v1 != Zero
    post = (r -> Zero) + (x -> v1)
  }
}

pred simpleexec[x:Exec] {
  /*
  e1: Wx=1
  e2: Rx=0
  */
  some disj e1, e2 : E {
    x.ev = e1 + e2
    x.sb = (e1 -> e2)
    x.cd = none->none
    x.ad = none->none
    x.dd = none->none
    x.W = e1
    x.R = e2
    x.F = none
    x.sthd = sq[e1 + e2]
    x.sloc = sq[e1 + e2]
    x.naL = none
    x.rf = none->none
    x.co = none->none
  }
}

run p1 for 5 Int, 
1 Prog, 3 Cmd, 2 Val, 2 Addr,
1 Exec, 2 E

run p2 for 5 Int, 
1 Prog, 7 Cmd, 2 Val, 4 Addr,
1 Exec, 4 E





