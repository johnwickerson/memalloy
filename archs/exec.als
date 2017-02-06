module exec[E]
open relations[E]

sig Exec {
  ev : set E,      // domain of all events
  W, R, F : set E, // writes, reads, fences
  IW : set E,      // initial writes
  naL : set E,     // events accessing non-atomic locations
  sb : E -> E,     // sequenced before
  ad : E -> E,     // address dependency
  dd : E -> E,     // data dependency
  cd : E -> E,     // control dependency
  sthd : E -> E,   // same thread (E.R.)
  sloc : E -> E,   // same location (partial E.R.)
  //////////////////////////////////////
  rf : E -> E,     // reads-from
  co : E -> E,     // coherence order
}{
  // ev captures all and only the events involved
  W + R + F + naL in ev
    
  // fences are disjoint from accesses
  no ((R + W) & F)

  // initial events are ordinary writes
  IW in W - R

  // at most one initial write per location
  (IW -> IW) & sloc in iden
    	
  // sequenced-before is intra-thread
  sb in sthd

  // sequenced-before is acyclic and transitive
  strict_partial_order[sb]

  // sequenced-before has the "N-free" property
  all a,b,c,d : ev | not (
	((b->d) + (a->d) + (a->c)) in sb and
      no (((a->b) + (b->c) + (c->d)) & *sb))
    
  // Event e2 has an "address dependency" on e1 if
  // location[e2] depends on valr[e1]. Therefore "(e1,e2) in ad"
  // only makes sense when e1 is a read and e2 is a read or a write
  // sequenced after e1.
  ad in (R -> (R + W)) & sb

  // Event e2 has a "data dependency" on e1 if valw[e2] depends
  // on valr[e1]. Therefore "(e1,e2) in data" only makes sense when
  // e1 is a read and e2 is a write sequenced after e1.
  dd in (R -> W) & sb
 
  // Event e2 has a "control dependency" on e1 if
  // valr[e1] is used to make a subsequent branching decision
  // and e2 follows that branch in program order. Therefore
  // "(e1,e2) in cd" only makes sense when e1 is a read and e2
  // is sequenced after e1.
  cd in (R -> E) & sb

  // Control dependencies are transitive. I don't think
  // we should necessarily say the same about other types
  // of dependencies
  //transitive[cd]

  // sthd is an equivalence relation among non-initial events
  is_equivalence[sthd, ev - IW]
    
  // loc is an equivalence relation among reads and writes
  is_equivalence[sloc, R + W]

  // naL contains zero or more sloc-classes
  naL . sloc = naL

  rf in sloc

  // co is acyclic and transitive
  strict_partial_order[co]

  // co is a union, over all atomic locations x, of strict
  // total orders on writes to x
  (co + ~co) = ((W - naL) -> (W - naL)) & sloc - iden
  	
}

pred withinit[X:Exec] {
  // rf connects each read to exactly one write
  X.rf in X.W one -> X.R

  // for every event that accesses a location, there is
  // exactly one initial event at the same location
  all e : X.(R+W) | one (e.(X.sloc) & X.IW)

  // initial writes have no co-predecessor
  all e : X.IW | no e.~(X.co)
}

pred withoutinit[X:Exec] {
  // rf connects each read to at most one write
  X.rf in X.W lone -> X.R
    
  // there are no initial writes
  no X.IW
}

fun ev [e:E, X:Exec] : set E { X.ev - e }
fun W [e:E, X:Exec] : set E { X.W - e }
fun IW [e:E, X:Exec] : set E { X.IW - e }
fun R [e:E, X:Exec] : set E { X.R - e }
fun F [e:E, X:Exec] : set E { X.F - e }
fun M [e:E, X:Exec] : set E { X.R + X.W - e }
fun naL [e:E, X:Exec] : set E { X.naL - e }

fun sb [e:E, X:Exec] : E->E { X.sb - (univ -> e) - (e -> univ) }
fun ad [e:E, X:Exec] : E->E { X.ad - (univ -> e) - (e -> univ) }
fun dd [e:E, X:Exec] : E->E { X.dd - (univ -> e) - (e -> univ) }
fun cd [e:E, X:Exec] : E->E { X.cd - (univ -> e) - (e -> univ) }
fun sthd [e:E, X:Exec] : E->E { X.sthd - (univ -> e) - (e -> univ) }
fun sloc [e:E, X:Exec] : E->E { X.sloc - (univ -> e) - (e -> univ) }
fun rf [e:E, X:Exec] : E->E { X.rf - (univ -> e) - (e -> univ) }
fun co [e:E, X:Exec] : E->E { X.co - (univ -> e) - (e -> univ) }

// Some synonyms
fun po [e:E, X:Exec] : E->E { sb[e,X] }
fun addr [e:E, X:Exec] : E->E { ad[e,X] }
fun ctrl [e:E, X:Exec] : E->E { cd[e,X] }
fun data [e:E, X:Exec] : E->E { dd[e,X] }
fun loc [e:E, X:Exec] : E->E { sloc[e,X] }
fun thd [e:E, X:Exec] : E->E { sthd[e,X] }
fun ext [e:E, X:Exec] : E->E { (X.ev -> X.ev) - sthd[e,X] }

fun fr_init[e:E, x:Exec] : E->E {
  (stor[R[e,x]] - (~(rf[e,x]) . (rf[e,x]))) . (sloc[e,x]) . (stor[W[e,x]])
}

fun fr[e:E, x : Exec] : E -> E {
  (fr_init[e,x] + (~(rf[e,x]) . (co[e,x]))) - iden
}

pred no_RMWs[e:E, x : Exec] {
  no (R[e,x] & W[e,x])
}

// sb, within each thread, is total
pred total_sb[e:E, x : Exec] {
  sthd[e,x] - iden in sb[e,x] + ~(sb[e,x])
}

/*
pred forced_co[e:E, x : Exec] {
  (imm[co[e,x]]) . (imm[co[e,x]]) . ~(imm[co[e,x]]) in
    (rc[rf[e,x]]) . (rc[(sb[e,x]) . (rc[~(rf[e,x])])])
}
*/

/*
pred no_if_zero[e:E, x:Exec] {
  // avoid "if(r==0)" in generated litmus test
  dom[cd[e,x]] in ran[rf[e,x]]
}
*/

/*************************/
/*      TESTS            */
/*************************/

pred messagepassing[x : Exec] {
  /*
  e1: Wx=1   e3: Ry=1
  e2: Wy=1   e4: Rx=0
  */
  some disj e1, e2, e3, e4 : E {
    x.ev = e1 + e2 + e3 + e4
    x.sb = (e1 -> e2) + (e3 -> e4)
    x.cd = (e3 -> e4)
    x.ad = none->none
    x.dd = none->none
    x.W = e1 + e2
    x.R = e3 + e4
    x.F = none
    x.sthd = sq[e1 + e2] + sq[e3 + e4]
    x.sloc = sq[e1 + e4] + sq[e2 + e3]
    x.naL = none
    x.rf = (e2 -> e3)
    x.co = none->none
  }
}

run messagepassing for 1 Exec, 4 E
