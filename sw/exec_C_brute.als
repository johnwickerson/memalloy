module exec_C_brute[E]
open ../relations[E]

pred wf[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  // ev captures all and only the events involved
  W + R + F + A + naL in ev
     
  // all events are reads or writes (or RMWs) or fences
  //ev in R + W + F
    
  // fences are disjoint from accesses
  no ((R + W) & F)
    
  // RMWs and fences are atomic
  (F + (R & W)) in A
    	
  // sequenced-before is intra-thread
  sb in sthd

  // sequenced-before is acyclic
  is_acyclic[sb]
    	
  // sequenced-before is transitive	
  transitive[sb]

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

  // sthd is an equivalence relation among all events
  is_equivalence[sthd, ev]
    
  // loc is an equivalence relation among reads and writes
  is_equivalence[sloc, R+W]

  // naL contains zero or more sloc-classes
  naL . sloc in naL
  
  // atomic events do not access non-atomic locations
  no (A & naL)
  // non-atomic reads do not access atomic locations
  R-A in naL
    

  // acquires, releases, and SC operations are all atomic
  acq + rel + sc in A
    
  // sc reads can acquire
  (R & sc) in acq

  // only reads and fences can acquire
  acq in (R + F)
    
  // sc writes can release
  (W & sc) in rel

  // only writes and fences can release
  rel in (W + F)
    
  // sc fences can acquire and release
  (F & sc) in (acq & rel)

  // rf maps each read from zero or one writes
  rf in (W lone -> R)
    		
  // rf edges preserve location
  rf in sloc

  // co is acyclic
  is_acyclic[co]
    
  // co is intransitive
  transitive[co]

  // co is a union, over all atomic locations x, of strict
  // total orders on writes to x
  (co + ~co) = ((W - naL) -> W) & sloc - iden
  	
}

fun fr[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] : E -> E {
  (((stor[R] - (~rf . rf)) . sloc . (stor[W])) + (~rf . co)) - iden
}

pred no_RMWs[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  no (R & W)
}

// sb, within each thread, is total
pred total_sb[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  sthd - iden in sb + ~sb
}

/*************************/
/*      TESTS            */
/*************************/

pred messagepassing[ev, W, R, F, A, naL, acq, rel, sc : set E, sb, ad, dd, cd, sthd, sloc, rf, co : E->E] {
  /*
  e1: Wx=1   e3: Ry=1
  e2: Wy=1   e4: Rx=0
  */
  some disj e1, e2, e3, e4 : E {
    ev = e1 + e2 + e3 + e4
    sb = (e1 -> e2) + (e3 -> e4)
    cd = (e3 -> e4)
    ad = none->none
    dd = none->none
    //A = e1 + e2 + e3 + e4
    W = e1 + e2
    R = e3 + e4
    F = none
    sthd = sq[e1 + e2] + sq[e3 + e4]
    sloc = sq[e1 + e4] + sq[e2 + e3]
    naL = none
    rf = (e2 -> e3)
    co = none->none
  }
}

run messagepassing for 4 E
