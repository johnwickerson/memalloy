open ../mappings/c11_strengthening_mo[E]
open ../sw/c11_simp[E] as M1

sig E {}

pred gp [
  X, X' : Exec_C, map : E->E
] {
        
  // we have a valid application of the mapping
  apply_map_c11[X, X']

  map = (X.ev <: iden)

  // The "weak" execution is inconsistent ...
  not(M1/consistent[none,X])
  M1/dead[none,X]
  // consider using "dead_efficient" here instead

  // But the "strong" execution is consistent (and not faulty)...
  M1/consistent[none,X']

  //hint[X]

  // Prefer solutions without RMWs
  no_RMWs[none,X]

  // Prefer solutions with total sb per thread
  total_sb[none,X]
}

pred hint[x : Exec_C] {
  /* Encoding the nice example:

a: W(na)a=1;   e: R(acq)x=1;
    |sb             |sb,cd
b: F(rlx);     f: R(na)a=1;
    |sb             |sb,cd
c: R(rlx)y=1;  g: W(rlx)y=1;
    |sb,cd
d: W(rlx)x=1;

   */
  some disj a,b,c,d,e,f,g : E { 
    x.ev = a+b+c+d+e+f+g
    x.R = c+e+f
    x.W = a+d+g
    x.A = b+c+d+e+g
    x.F = b
    x.naL = a+f
    x.acq = e
    x.rel = none
    x.sc = none
    x.dd = none->none
    x.ad = none->none
    x.cd = ^((c->d) + (e->f) + (f->g))
    x.sb = ^((a->b) + (b->c) + (c->d)) + ^((e->f) + (f->g))
    x.rf = (a->f) + (d->e) + (g->c)
    x.co = none->none 
  } 
}

run gp for 2 Exec, 6 E, 3 Int expect 0
// 5h (glucose, benjamin)


run gp for 2 Exec, 7 E, 3 Int expect 1
// 46s (glucose benjamin)
