module exec[E]
open relations[E]

sig Exec {
  ev : set E, // domain of all events
  W, R, F : set E, // writes, reads, fences
  I : set E, // initial writes
  naL : set E, // events accessing non-atomic locations
  sb : E -> E, // sequenced before
  ad : E -> E, // address dependency
  dd : E -> E, // data dependency
  cd : E -> E, // control dependency
  sthd : E -> E, // same thread (E.R.)
  sloc : E -> E, // same location (partial E.R.)
  //////////////////////////////////////
  rf : E -> E,  // reads-from
  co : E -> E,  // coherence order
}{
  // ev captures all and only the events involved
  W + R + F + naL in ev
    
  // fences are disjoint from accesses
  no ((R + W) & F)

  // initial events are writes
  I in W - R

  // at most one initial write per location
  (I -> I) & sloc in iden
    	
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
  is_equivalence[sthd, ev - I]
    
  // loc is an equivalence relation among reads and writes
  is_equivalence[sloc, R + W]

  // naL contains zero or more sloc-classes
  naL . sloc = naL

  // rf in W one -> R
  rf in sloc

  // co is acyclic and transitive
  strict_partial_order[co]

  // co is a union, over all atomic locations x, of strict
  // total orders on writes to x
  (co + ~co) = ((W - naL) -> (W - naL)) & sloc - iden
  	
}

pred withinit[x:Exec] {
  // rf connects each read to exactly one write
  x.rf in x.W one -> x.R

  // for every event that accesses a location, there is
  // exactly one initial event at the same location
  all e : x.(R+W) | one (e.(x.sloc) & x.I)

  // initial writes have no co-predecessor
  all e : x.I | no e.~(x.co)
}

pred withoutinit[x:Exec] {
  // rf connects each read to at most one write
  x.rf in x.W lone -> x.R
  no x.I
}

fun fr_init[x:Exec] : E->E {
  (stor[x.R] - (~(x.rf) . (x.rf))) . (x.sloc) . (stor[x.W])
}

fun fr[x : Exec] : E -> E {
  (fr_init[x] + (~(x.rf) . (x.co))) - iden
}

fun poloc[x : Exec] : E -> E {
  x.sb & x.sloc
}

pred Uniproc[x : Exec] {
  let com = x.rf + fr[x] + x.co |
  is_acyclic[poloc[x] + com]
}

pred no_RMWs[x : Exec] {
  no x.(R & W)
}

// sb, within each thread, is total
pred total_sb[x : Exec] {
  x.sthd - iden in x.sb + ~(x.sb)
}

pred forced_co[x : Exec] {
  (imm[x.co]) . (imm[x.co]) . ~(imm[x.co]) in
    (rc[x.rf]) . (rc[(x.sb) . (rc[~(x.rf)])])
}

pred forced_co_init[x : Exec] {
  ~(imm[x.co]) . (imm[x.co]) . (imm[x.co]) . (imm[x.co]) . ~(imm[x.co]) in
    (rc[x.rf]) . (rc[(x.sb) . (rc[~(x.rf)])])
}

pred forced_co_efficient[x : Exec] {
  x.co in (rc[x.rf]) . (rc[(x.sb) . (rc[~(x.rf)])])
}

pred no_if_zero[x:Exec] {
  // avoid "if(r==0)" in generated litmus test
  dom[x.cd] in ran[x.rf]
}

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
