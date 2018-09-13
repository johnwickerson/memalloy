module exec[E]
open relations[E]

sig Exec {
  EV : set E,      // domain of all events
  W, R, F : set E, // writes, reads, fences
  IW : set E,      // initial writes
  P : set E,       // persistent events
  sb : E->E,       // sequenced before
  ad,cd,dd : E->E, // address, control, data dependencies
  atom : E->E,     // atomicity relation
  sthd : E->E,     // same thread (partial E.R.)
  sloc : E->E,     // same location (partial E.R.)
  //////////////////////////////////////
  rf : E->E,       // reads-from
  co : E->E,       // coherence order
  nvo : E->E,      // non-volatile order
}{
  // EV captures all and only the events involved
  W + R + F in EV
    
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

  /*
  // sequenced-before has the "N-free" property
  all a,b,c,d : EV | not (
	((b->d) + (a->d) + (a->c)) in sb and
      	no (((a->b) + (b->c) + (c->d)) & *sb))
  */

  // sequenced-before is total within a thread
  sthd in *sb + ~*sb /* TODO: drop this constraint */

  // sthd is an equivalence relation among non-initial events
  is_equivalence[sthd, EV - IW]
    
  // loc is an equivalence relation among reads and writes
  is_equivalence[sloc, R + W]

  rf in sloc

  // co is acyclic and transitive
  strict_partial_order[co]

  // co is a union, over all locations x, of strict
  // total orders on writes to x
  (co + ~co) = (W -> W) & sloc - iden

  // nvo is prefix-closed with respect to persistent events
  (nvo . P) in P

  // reads are vacuously persistent
  R in P		
		
  // nvo is a total order over all events that affect the non-volatile
  // memory (i.e. all writes and certain fences)
  strict_partial_order[nvo]
  nvo in sq[W + F]
  (sq[W] - iden) in (nvo + ~nvo)	
		
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
  // "(e1,e2) in cd" only makes sense when e1 is a read (or possibly
  // a store-exclusive) and e2 is sequenced after e1.
  let XW = ran[atom] |
  cd in ((R + XW) -> EV) & sb

  // the atom relation relates a read/write pair in program order
  atom in (R->W) & sb & sloc
    
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

fun addsb[e:PTag->E, X:Exec, F:set E] : E->E {
  *(sb[e,X]) . (stor[F]) . *(sb[e,X]) }
  

// Perturbation Tags are an idea due to Daniel Lustig et al.
// (ASPLOS'17, http://dl.acm.org/citation.cfm?id=3037723)
abstract sig PTag {}
one sig rm_EV extends PTag {}
one sig rm_ad extends PTag {}
one sig rm_cd extends PTag {}
one sig rm_dd extends PTag {}
one sig rm_atom extends PTag {}

fun rm_EV_rel[e:PTag->E, r:E->E] : E->E {
  (univ - e[rm_EV]) <: r :> (univ - e[rm_EV])
}

fun EV [e:PTag->E, X:Exec] : set E { X.EV - e[rm_EV] }
fun W [e:PTag->E, X:Exec] : set E { X.W - e[rm_EV] }
fun IW [e:PTag->E, X:Exec] : set E { X.IW - e[rm_EV] }
fun R [e:PTag->E, X:Exec] : set E { X.R - e[rm_EV] }
fun F [e:PTag->E, X:Exec] : set E { X.F - e[rm_EV] }

fun sb [e:PTag->E, X:Exec] : E->E { rm_EV_rel[e, X.sb] }
fun sthd [e:PTag->E, X:Exec] : E->E { rm_EV_rel[e, X.sthd] }
fun sloc [e:PTag->E, X:Exec] : E->E { rm_EV_rel[e, X.sloc] }
fun rf [e:PTag->E, X:Exec] : E->E { rm_EV_rel[e, X.rf] }
fun co [e:PTag->E, X:Exec] : E->E { rm_EV_rel[e, X.co] }

fun fr [e:PTag->E, X:Exec] : E->E {
  let fr_base = ((X.R -> X.W) & X.sloc) - (~(X.rf) . *~(X.co)) |
  rm_EV_rel[e, fr_base]
}


fun ad [e:PTag->E, X:Exec] : E->E {
  rm_EV_rel[e, (univ - e[rm_ad]) <: X.ad] }
fun dd [e:PTag->E, X:Exec] : E->E {
  rm_EV_rel[e, (univ - e[rm_dd]) <: X.dd] }
fun cd [e:PTag->E, X:Exec] : E->E {
  rm_EV_rel[e, (univ - e[rm_cd]) <: X.cd] }
fun atom[e:PTag->E, X:Exec] : E->E {
  (univ - e[rm_EV] - e[rm_atom]) <: X.atom :> (univ - e[rm_EV]) }
