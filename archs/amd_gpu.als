/********************************************************/
/* Executions on a GPU                                  */
/********************************************************/
open util/relation
module amd_gpu

sig Loc {}
sig Val {}
one sig Zero extends Val {}

abstract sig Hygiene, Freshness {}
one sig CLEAN, DIRTY extends Hygiene {}
one sig VALID, INVALID extends Freshness {}

fun glom[r1,r2 : univ -> univ] : univ -> univ {
  r1 + (univ.r1 -> r2.univ) + r2
}

sig MemEntry {
  val : Val,
  pco : Inst -> Inst
}

sig CacheEntry extends MemEntry {
  hy : Hygiene,
  fr : Freshness,
}

sig LState {
  cac : Loc -> lone CacheEntry
}

sig GState { 
  mem : Loc -> one MemEntry,
  locked : set Loc
}

abstract sig Action {
  g_pre, g_post : GState,
  l_pre, l_post : LState,
  l_other : LState,
  then : set Action,
  same_thd : set Action,
  same_wg : set Action,
  loc : set Loc,
}{
  complete[@then, Action] 
  transitive[@then]
  acyclic[@then, Action]
  equivalence[@same_wg, Action]
  equivalence[@same_thd, Action]
  @same_thd in @same_wg
}

sig EnvFlush extends Action {}{

  // loc is dirty in L1
  all x : Loc | x in loc implies l_pre.cac[x].hy = DIRTY

  // loc is not locked in global mem
  no (loc & g_pre.locked)

  // update loc in global mem
  all x : Loc | 
    x in loc => 
      g_post.mem[x].val = l_pre.cac[x].val and
      g_post.mem[x].pco = glom[g_pre.mem[x].pco, l_pre.cac[x].pco] // ghost
    else 
      g_post.mem[x] = g_pre.mem[x]

  // global mem otherwise unchanged
  g_post.locked = g_pre.locked

  // make loc L1 cachelines clean but otherwise unchanged
  all x : Loc | 
    x in loc => 
      l_post.cac[x].hy = CLEAN and
      l_post.cac[x].fr = l_pre.cac[x].fr and
      l_post.cac[x].val = l_pre.cac[x].val
   else 
     l_post.cac[x] = l_pre.cac[x]

}

sig EnvFetch extends Action {} {

  // global mem is unchanged
  g_post = g_pre

  // loc is not locked in global mem
  no (loc & g_pre.locked)

  // loc is not dirty in L1
  all x : Loc | x in loc implies l_pre.cac[x].hy != DIRTY

  // update loc's L1 cacheline, and make it clean and valid
  all x : Loc | 
    x in loc =>
      l_post.cac[x].val = g_pre.mem[x].val and
      l_post.cac[x].hy = CLEAN and
      l_post.cac[x].fr = VALID and
      l_post.cac[x].pco = (none -> none) // ghost
  else 
    l_post.cac[x] = l_pre.cac[x]

}

sig EnvInval extends Action {} {

  // global mem is unchanged
  g_post = g_pre

  // make loc L1 cachelines invalid but otherwise unchanged
  all x : Loc | 
    x in loc =>
      l_post.cac[x].hy = l_pre.cac[x].hy and
      l_post.cac[x].fr = INVALID and
      l_post.cac[x].val = l_pre.cac[x].val and
      l_post.cac[x].pco = l_pre.cac[x].pco
    else 
      l_post.cac[x] = l_pre.cac[x]
  
}

abstract sig Inst extends Action {
  rval : lone Val,
  wval : lone Val
}

sig Load extends Inst {
  src : lone Inst
} {

  one loc
  one rval
  no wval

  // L1 cache is unchanged
  l_post.cac = l_pre.cac

  // global mem is unchanged
  g_post = g_pre

  // loc is valid in L1 cache
  l_pre.cac[loc].fr = VALID

  // value matches up
  l_pre.cac[loc].val = rval

}

sig Store extends Inst {}{

  one loc
  one wval
  no rval

  // global mem is unchanged
  g_post = g_pre

  // update loc in L1 cache
  l_post.cac[loc].val = wval
  l_post.cac[loc].hy = DIRTY
  l_post.cac[loc].fr = VALID
  l_post.cac[loc].pco = glom[l_pre.cac[loc].pco, this->this]

  // rest of L1 is unchanged
  all x : Loc | x != loc implies l_post.cac[x] = l_pre.cac[x]
}

sig IncL1 extends Inst {
  src : lone Inst
}{
  one loc
  one wval
  one rval

  // loc is valid in L1 cache
  l_pre.cac[loc].fr = VALID

  // value matches up
  l_pre.cac[loc].val = rval

  // update loc in L1 cache
  l_post.cac[loc].val = wval
  l_post.cac[loc].hy = DIRTY
  l_post.cac[loc].fr = VALID
  l_post.cac[loc].pco = glom[l_pre.cac[loc].pco, this->this]

  // rest of L1 is unchanged
  all x : Loc | x != loc implies l_post.cac[x] = l_pre.cac[x]

  // global mem is unchanged
  g_post = g_pre
}

sig Inval extends Inst {}{

  no loc
  no wval
  no rval
 
  // no location is valid in L1
  VALID not in l_pre.cac[Loc].fr
 
  // nothing is changed
  l_post.cac = l_pre.cac
  g_post = g_pre

}

sig RemInval extends Inst {}{

  no loc
  no wval
  no rval

  // no location is valid in the other L1
  VALID not in l_other.cac[Loc].fr
 
  // nothing is changed
  l_post.cac = l_pre.cac
  g_post = g_pre

}

sig Flush extends Inst {}{

  no loc
  no rval
  no wval

  // no location is dirty in L1
  DIRTY not in l_pre.cac[Loc].hy
 
  // nothing is changed
  l_post.cac = l_pre.cac
  g_post = g_pre
}

sig RemFlush extends Inst {}{

  no loc
  no rval
  no wval

  // no location is dirty in the other L1
  DIRTY not in l_other.cac[Loc].hy
 
  // nothing is changed
  l_post.cac = l_pre.cac
  g_post = g_pre
}

sig Lock extends Inst {} {

  one loc
  no rval
  no wval

  // L1 cache is unchanged
  l_post.cac = l_pre.cac

  // global mem is unchanged
  g_post.mem = g_pre.mem

  // loc is unlocked before, and locked after
  loc not in g_pre.locked
  g_post.locked = g_pre.locked + loc
}

sig Unlock extends Inst {} {

  one loc
  no rval
  no wval

  // L1 cache is unchanged
  l_post.cac = l_pre.cac

  // global mem is unchanged
  g_post.mem = g_pre.mem

  // loc is locked before, and unlocked after
  loc in g_pre.locked
  g_post.locked = g_pre.locked - loc
}

pred same_loc[i1, i2 : Inst] {
  one i1.loc
  one i2.loc
  i1.loc = i2.loc
}

pred initial_gstate [gs : GState] {
  all x : Loc | gs.mem[x].val = Zero
}

pred initial_lstate[ls : LState] {
  no (ls.cac)
}

pred final_lstate[ls : LState] {
  DIRTY not in ls.cac[Loc].hy
}

pred is_first_action[a : Action] {
  a not in Action.then
}

pred is_last_action[a : Action] {
  no (a.then)
}

pred is_first_action_in_wg[a : Action] {
  no b : Action | (b -> a) in then & same_wg
}

pred is_last_action_in_wg[a : Action] {
  no b : Action | (a -> b) in then & same_wg
}

pred is_next_action_in_other_wg[a,b : Action] {
  (a -> b) in (then - same_wg) - ((then - same_wg) . then)
}

pred is_prev_action_in_other_wg[a,b : Action] {
  (b -> a) in (then - same_wg) - (then . (then - same_wg))
}

pred globally_consecutive[a,b : Action] {
  (a -> b) in then - (then . then)
}

pred locally_consecutive[a,b : Action] {
  (a -> b) in (then & same_wg) - ((then & same_wg) . then)
}

pred sb_imm[a,b : Inst] {
  (a -> b) in (Inst <: (then & same_thd) :> Inst) - 
             ((Inst <: (then & same_thd) :> Inst) . then)
}

pred consecutive_lock_unlock[a : Lock, b : Unlock] {
  (a -> b) in (Lock <: then :> Unlock) - 
             ((Lock <: then :> Unlock) . then)
}

fun derived_rf : Inst -> Inst {
  let my_src = (Load <: src) + (IncL1 <: src) |
    ~my_src
}

fun derived_co : Inst -> Inst {
  ^({i1, i2 : Inst | 
      some x : Loc | 
        all a : Action | is_last_action[a] implies
          (i1 -> i2) in a.g_post.mem[x].pco - iden})
}

pred consistent {

  // We start in an "initial" state
  all a : Action | 
    is_first_action[a] implies initial_gstate[a.g_pre]

  // Each workgroup starts in an "initial" state
  all a : Action | 
    is_first_action_in_wg[a] implies initial_lstate[a.l_pre]

  // Each workgroup ends in a "final" state
  all a : Action |
    is_last_action_in_wg[a] implies final_lstate[a.l_post]

  // Every Lock is followed by an Unlock in the same thread
  all a : Lock |
    some b : Unlock | 
      (a -> b) in then & same_thd and a.loc = b.loc

  // Each consecutive (Lock, Unlock) pair is in the same thread
  all a,b : Action |
    consecutive_lock_unlock[a,b] implies (a -> b) in same_thd

  // The src map agrees with the read/written values
  all a : Load |
    no a.src => a.rval = Zero else 
                a.rval = a.src.wval

  // Consecutive actions in a work-group share local state
  all a,b : Action | 
    locally_consecutive[a,b] implies a.l_post = b.l_pre

  // Consecutive global actions share global state
  all a,b : Action |
    globally_consecutive[a,b] implies a.g_post = b.g_pre
 
  // The "other state" coincides with pre-state of 
  // the next action in the other work-group
  all a,b : Action |
    is_next_action_in_other_wg[a,b] implies a.l_other = b.l_pre

  // The "other state" coincides with post-state of 
  // the previous action in the other work-group
  all a,b : Action |
    is_prev_action_in_other_wg[a,b] implies a.l_other = b.l_post

  // There are no spare states floating around
  LState in Action.(l_pre + l_post)
  GState in Action.(g_pre + g_post)

  // There are no spare memory entries floating around
  MemEntry in (GState.mem[Loc] + LState.cac[Loc])

  /*
  // Show an "immediate then" relation
  some imm_then : Action -> Action {
    imm_then in then
    no (imm_then & (imm_then . ^imm_then))
    (Action -> Action) in *imm_then + ~*imm_then
  }
  */  
}

pred postcondition[x : Loc, v : Val] {

   all a : Action |
    is_last_action[a] implies a.g_post.mem[x].val = v

}

pred mp_forced {

  consistent

  some disj x, y : Loc |
  some disj a0, a1, a2, a3, a4, a5, a6, a7, a8, a9 : Action {

    (a0 -> a1) + (a1 -> a2) + (a2 -> a3) + (a3 -> a4) + (a4 -> a5) +
    (a5 -> a6) + (a6 -> a7) + (a7 -> a8) + (a8 -> a9) in then
 
    (a0 -> a1) + (a1 -> a7) + (a7 -> a8) + (a8 -> a9) in same_thd
    (a2 -> a3) + (a3 -> a4) + (a4 -> a5) + (a5 -> a6) in same_thd
    (a0 -> a2) not in same_thd

    same_thd = same_wg

    // Alloy can't add extra instructions
    Inst in a0 + a2 + a4 + a5 + a8 + a9

    a0 in Inval
    a1 in EnvFetch && a1.loc = x  // environment transition
    a2 in Store && a2.loc = x && a2.wval != Zero
    a3 in EnvFlush && a3.loc = x  // environment transition
    a4 in Flush
    a5 in Store && a5.loc = y && a5.wval != Zero
    a6 in EnvFlush && a6.loc = y  // environment transition
    a7 in EnvFetch && a7.loc = y  // environment transition
    a8 in Load && a8.loc = y && a8.rval = a5.wval
    a9 in Load && a9.loc = x && a9.rval = Zero
  }
}

pred mp {

  consistent

  some disj x, y : Loc |
  some disj a0, a1, a2, b0, b1, b2 : Action {

    (a0 -> a1) + (a1 -> a2) in then & same_thd
    (b0 -> b1) + (b1 -> b2) in then & same_thd

    // Alloy is not allowed to add extra instructions
    Inst in a0 + a1 + a2 + b0 + b1 + b2

    a0 in Store && a0.loc = x && a0.wval != Zero
    a1 in Flush
    a2 in Store && a2.loc = y && a2.wval != Zero
    b0 in Inval
    b1 in Load  && b1.loc = y && b1.rval != Zero
    b2 in Load  && b2.loc = x && b2.rval = Zero
  }
}

pred atom_forced_1 {

  consistent

  some x : Loc |
  some val1, val2 : Val | disj[Zero, val1, val2] and
  some disj a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11 : Action {

    (a0 -> a1) + (a1 -> a2) + (a2 -> a3) + (a3 -> a4) + 
    (a4 -> a5) + (a5 -> a6) + (a6 -> a7) + (a7 -> a8) + 
    (a8 -> a9) + (a9 -> a10) + (a10 -> a11) in then

    (a0 -> a3) + (a3 -> a4) + (a4 -> a6) + (a6 -> a7) + 
    (a7 -> a8) + (a8 -> a9) + (a9 -> a10) in same_thd
    (a1 -> a2) + (a2 -> a5) + (a5 -> a11) in same_thd
    (a0 -> a1) not in same_thd

    same_thd = same_wg

    // Alloy can't add extra instructions
    Inst in a2 + a3 + a4 + a6 + a8 + a9
	
    a0 in EnvFetch  && a0.loc = x  // environment transition
    a1 in EnvFetch  && a1.loc = x  // environment transition
    a2 in IncL1     && a2.loc = x && a2.rval = Zero && a2.wval = val1
	a3 in Lock      && a3.loc = x
    a4 in Flush
    a5 in EnvInval  && a5.loc = x  // environment transition
    a6 in Store     && a6.loc = x && a6.wval = val2
    a7 in EnvInval  && a7.loc = x  // environment transition
    a8 in RemInval
    a9 in Unlock    && a9.loc = x
    a10 in EnvFlush && a10.loc = x // environment transition
    a11 in EnvFlush && a11.loc = x // environment transition

    postcondition[x,val1]

  }
}

pred atom_forced_2 {

  consistent

  some x : Loc |
  some val1, val2 : Val | disj[Zero, val1, val2] and
  some disj a0, a1, a2, a3, a4, a5, a6, a7, a8 : Action {

    (a0 -> a1) + (a1 -> a2) + (a2 -> a3) + (a3 -> a4) + 
    (a4 -> a5) + (a5 -> a6) + (a6 -> a7) + (a7 -> a8) in then

    (a0 -> a1) + (a1 -> a2) + (a2 -> a3) + (a3 -> a4) + 
    (a4 -> a7) in same_thd
    (a5 -> a6) + (a6 -> a8) in same_thd
    (a0 -> a5) not in same_thd

    same_thd = same_wg

    // Alloy can't add extra instructions
    Inst in a0 + a1 + a2 + a3 + a4 + a6

    a0 in Lock      && a0.loc = x
    a1 in Flush
    a2 in Store     && a2.loc = x && a2.wval = val2
    a3 in RemInval
    a4 in Unlock    && a4.loc = x
    a5 in EnvFetch  && a5.loc = x  // environment transition
    a6 in IncL1     && a6.loc = x && a6.rval = Zero && a6.wval = val1
    a7 in EnvFlush  && a7.loc = x  // environment transition
    a8 in EnvFlush  && a8.loc = x  // environment transition

    postcondition[x,val1]

  }
}

pred atom {

  consistent

  some x : Loc |
  some val1, val2 : Val | disj[Zero, val1, val2] and
  some disj a0, b0, b1, b2, b3, b4 : Action {

    (b0 -> b1) + (b1 -> b2) + (b2 -> b3) + (b3 -> b4) in then & same_thd

    // Alloy can't add extra instructions
    Inst in a0 + b0 + b1 + b2 + b3 + b4

    a0 in IncL1  && a0.loc = x && a0.rval = Zero && a0.wval = val1
	b0 in Lock   && b0.loc = x
    b1 in Flush
    b2 in Store  && b2.loc = x && b2.wval = val2
    b3 in RemInval
    b4 in Unlock && b4.loc = x

    postcondition[x,val1]

  }
}

run mp_forced for 
3 GState,
7 LState,
5 MemEntry,
2 Val,
2 Loc,
10 Action
expect 1
// 10 seconds (plingeling on babillion)

run mp for 
3 GState,
7 LState,
5 MemEntry,
2 Val,
2 Loc,
10 Action
expect 1
// 11 mins (plingeling on babillion)

run atom_forced_1 for 
4 GState,
8 LState,
7 MemEntry,
3 Val,
1 Loc,
12 Action
expect 1
// 33 seconds (plingeling on babillion)


run atom_forced_2 for 
4 GState,
6 LState,
6 MemEntry,
3 Val,
1 Loc,
9 Action
expect 1
// 2 seconds (plingeling on babillion)

run atom for 
4 GState,
6 LState,
6 MemEntry,
3 Val,
1 Loc,
9 Action
// 6 seconds (plingeling on babillion)

