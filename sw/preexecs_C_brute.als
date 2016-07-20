module preexec_C_brute[E]
open prog_C
open exec_C_brute[E]

pred failed_cas[a: Inst, pi : Inst -> E, rf : E -> E] {
  a in Cas and a.rval != a.pi.~rf.~pi.wval
}

pred successful_cas[a: Inst, pi : Inst -> E, rf : E -> E] {
  a in Cas and a.rval = a.pi.~rf.~pi.wval
}

pred apply_pi[
  pi : Inst -> E, p : Prog, ev, W, R, F, A, naL, acq, rel, sc : set E, 
  sb, ad, dd, cd, sthd, sloc, rf, co : E->E, enabled : set Cmd, post : Addr -> Val
] {
  pi in (enabled & Inst) one->one ev
  //ev = E
  dom[pi] = enabled & Inst
  ran[pi] = ev

  check_enabled_actions[enabled, p, post]

  // the only registers are those that are loaded/exchanged into
  //Reg in (Load + Cas) <: reg

  all a : enabled & Inst {

    // read => load or failed cas
    a.pi in R-W iff 
      (a in Load or failed_cas[a,pi,rf])
  
    // write => store
    a.pi in W-R iff 
      a in Store
   
    // RMW => successful cas
    a.pi in R&W iff 
      successful_cas[a,pi,rf]
 
    // fence => fence
    a.pi in F iff a in Fence

    // atomic locations
    one a.loc implies (a.loc in NALoc iff a.pi in naL)

    // atomic operations
    a.pi in A iff a in p.insts_atomic

    // memory order: acquire
    a.pi in acq iff (a in p.insts_acq and not failed_cas[a,pi,rf])
 
    // memory order: release
    a.pi in rel iff (a in p.insts_rel and not failed_cas[a,pi,rf])

    // memory order: sc
    a.pi in sc iff (a in p.insts_sc and not failed_cas[a,pi,rf])
  }

  // same thread
  all a, b : enabled & Inst |
    ((a -> b) in ~^(child[p]) . ^(child[p]))
    iff 
    (a.pi -> b.pi) in sthd
    
  // same location
  all a, b : enabled & Inst |
    (one a.loc && one b.loc && a.loc = b.loc) 
    iff
    (a.pi -> b.pi) in sloc
 
  // control dependencies 
  all a, b : enabled & Inst | 
    (some g : If & enabled {
       a in Load + Cas
       a.reg = g.reg 
       b in g.^(child[p] :> enabled)
    })
    iff
    (a.pi -> b.pi) in cd
   
  // address dependencies
  all a, b : enabled & Inst |
    (a in Load + Cas && a.reg in b.fake_loc_deps)
    iff
    (a.pi -> b.pi) in ad

  // data dependencies
  all a, b : enabled & Inst |
    (a in Load + Cas && a.reg in b.fake_wval_deps)
    iff
    (a.pi -> b.pi) in dd
 
  // sequenced before
  all cmd : Seq |
    all a : cmd.(p.fst).*(child[p]) & enabled & Inst |
      all b : cmd.(p.snd).*(child[p]) & enabled & Inst |
        (a.pi -> b.pi) in sb
  all cmd : Unseq |
    all a : cmd.(p.fst).*(child[p]) & enabled & Inst |
      all b : cmd.(p.snd).*(child[p]) & enabled & Inst |
        (a.pi -> b.pi) not in sb + ~sb

  // reads-from
  all a: enabled & (Store + Cas) | 
    all b: enabled & (Load + Cas) |
      (a.loc = b.loc && a.wval = post[b.reg]) iff (a.pi -> b.pi) in rf
  all b: enabled & (Load + Cas) |
    (post[b.reg] = Zero) iff no b.pi.~rf

  // final writes
  all l : Loc | (
    // "a" is the last write to location "l" so post[l] takes 
    // the value that "a" writes
    some a : enabled & (Store + Cas) { 
      a.loc = l
      co_terminator[a.pi, W, sloc, co]
      post[l] = a.wval
    }
  ) or (
    // location "l" is never written, so post[l] remains 0
    (all a : enabled & (Store + Cas) | a.loc != l) and post[l] = Zero
  )
} 

pred co_terminator[e:E, W:set E, sloc:E->E, co:E->E] {
  all e' : W | 
    (e -> e') in sloc - iden implies (e -> e') not in co
}

pred is_canonical_prog[
  p : Prog, rf : E->E, enabled : set Inst, post : Addr -> Val
] {
  // every instruction is enabled
  p.cm in enabled

  // every CAS succeeds
  all a : Cas | post[a.reg] = a.rval
}

pred cand_exec[
  p : Prog, ev, W, R, F, A, naL, acq, rel, sc : set E, 
  sb, ad, dd, cd, sthd, sloc, rf, co : E->E, enabled : set Cmd, post : Addr -> Val
] {
  some pi : Inst -> E | 
    apply_pi[pi,p,ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co,enabled,post]
}

pred cand_exec'[
  p : Prog, ev, W, R, F, A, naL, acq, rel, sc : set E, 
  sb, ad, dd, cd, sthd, sloc, rf, co : E->E, post : Addr -> Val
] {
  some enabled : set Cmd | 
    cand_exec[p,ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co,enabled,post]
}

pred p1 [
  p : Prog, ev, W, R, F, A, naL, acq, rel, sc : set E, 
  sb, ad, dd, cd, sthd, sloc, rf, co : E->E, enabled : set Cmd, post : Addr -> Val
] {
  cand_exec[p,ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co,enabled,post]
  is_canonical_prog[p,rf,enabled,post]
  messagepassing[ev,W,R,F,A,naL,acq,rel,sc,sb,ad,dd,cd,sthd,sloc,rf,co]
}

run p1 for 5 Int, 
1 Prog, 7 Cmd, 2 Val, 2 Loc, 2 Reg, 4 E






