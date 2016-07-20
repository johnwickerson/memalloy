module prog
open util/relation

sig Val {}
one sig Zero extends Val {}

abstract sig Addr {}             // locations + registers
sig Reg extends Addr {}          // registers
abstract sig Loc extends Addr {} // locations
sig NALoc, ALoc extends Loc {}   // non-atomic, atomic locations

sig Prog {
  cm : set Cmd,         // all commands in the program
  top : set Cmd,        // top-level command of each thread
  fst,snd : Cmd -> Cmd, // the two operands of sequenced/unsequenced composition
  then : Cmd -> Cmd     // body of if-statement
} {

  top in cm
  fst in (cm & (Seq + Unseq)) -> one Cmd
  snd in (cm & (Seq + Unseq)) -> one Cmd
  then in (cm & If) -> one Cmd 

  // the two operands of a seq/unseq are not the same
  no (~fst . snd) & iden

  // the child relation is forest-like
  acyclic[child[this], cm]
  all c : cm - top | one c.~(child[this])
  all c : top | no c.~(child[this])
  
  // each register is written at most once
  all disj i, j : cm & (Load + Cas) | i.reg != j.reg

  // different writes to the same location store different values
  all disj i, j : cm & (Store + Cas) | 
    i.loc = j.loc implies i.wval != j.wval

  // Don't do the same test twice (e.g. "if (r==v) if (r==v)"). 
  // It's redundant but not harmful.
  all i,j : cm & If | (i -> j) in ^then implies (i.reg != j.reg or i.rval != j.rval)

  // every if-test reads a register that is written earlier in the same thread
  all i : If | some j : Load + Cas | 
    i.reg = j.reg && (j -> i) in sequenced[this]

}

fun child[P:Prog] : Cmd -> Cmd {
  P.fst + P.snd + P.then
}

abstract sig Cmd {
  loc : lone Loc,  // location accessed
  reg : lone Reg,  // register accessed
  rval : lone Val, // value read
  wval : lone Val  // value written
}

sig Unseq extends Cmd {}{ // unsequenced composition
  no loc
  no reg
  no rval
  no wval
}

sig Seq extends Cmd {}{  // sequenced composition
  no loc
  no reg
  no rval
  no wval
}

sig If extends Cmd {} { // one-armed if-statement
  no loc
  one reg
  one rval
  no wval
  // never test for zero, because we can't tell whether
  // the zero arose from a load that reads zero, or from
  // a failure to execute the load at all.
  rval != Zero
}

abstract sig Inst extends Cmd { // instruction
  fake_loc_deps : set Reg,
  fake_wval_deps : set Reg
} {
  no loc implies no fake_loc_deps
  no wval implies no fake_wval_deps
}

sig Load extends Inst {} {
  one loc
  one reg
  no rval
  no wval
}

sig Store extends Inst {} {
  one loc
  no reg
  one wval
  no rval
  // never write zero
  wval != Zero
}

sig Cas extends Inst {} {
  one loc
  one reg
  one wval
  one rval
  // never write zero
  wval != Zero
}

sig Fence extends Inst {} {
  no loc
  no reg
  no wval
  no rval
}

fun sequenced[P:Prog] : Cmd -> Cmd {
  *~(child[P]).~(P.fst).(P.snd).*(child[P])
}

fun imm_sequenced[P:Prog] : Cmd -> Cmd {
  ~(P.fst).(P.snd)
}

fun same_thread[P:Prog] : Cmd -> Cmd {
  *~(child[P]).*(child[P])
}

pred control_dep[P:Prog, i1,i3:Inst] {
  some i2 : If {
    (i1 -> i2) in sequenced[P]
    (i2 -> i3) in *(child[P])
    i1.reg = i2.reg
  }
}

pred check_enabled_actions
  [enabled : set Cmd, P : Prog, post : Addr -> Val] 
{
  // The enabled actions are a subset of the program's actions
  enabled in P.cm
 
  // the postcondition assigns a final value to every address
  post in Addr ->one Val

  // for every if-statement...
  all i : If |
    // the test condition satisfies the postcondition 
    //     and the if-statement is enabled
    // iff the then-branch is enabled
    (post[i.reg] = i.rval && i in enabled) iff (i.(P.then) in enabled)

  // If a load is not executed, it cannot set its register
  all i : P.cm & (Load + Cas) - enabled | post[i.reg] = Zero

  // statements not inside an if-statement are always enabled
  all i : P.cm | no i.~*(child[P]).~(P.then) implies i in enabled
  
}

