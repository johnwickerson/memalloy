/* Models from paper "Repairing SC in C/C++11 */

module c11_repairing[E]
open c11_base[E]

fun rb[x:Exec_C] : E->E {
  (stor[x.R]) . ~(x.rf) . (x.co)
}

// Batty et al:
pred consistent_batty[x:Exec_C]{
  HbCom[x]
  NaRf[x]
  let Esc = stor[x.sc] |
  let psc = Esc . (rc[Fsb[x]]) . ((hb[x]) + x.co + rb[x]) . (rc[sbF[x]]) . Esc |
  is_acyclic[psc]
}

// Repairing C11/Power mapping:
pred consistent1[x:Exec_C]{
  HbCom[x]
  NaRf[x]
  let Esc = stor[x.sc] |
  let shsb = (x.sb) . (hb[x]) . (x.sb) |
  let psc = Esc . (rc[Fsb[x]]) .((x.sb) + shsb + x.rf + x.co + rb[x]) . (rc[sbF[x]]) . Esc |
  is_acyclic[psc]
}

// Stronger SC fences:
pred consistent2[x:Exec_C]{
  HbCom[x]
  NaRf[x]
  let Esc = stor[x.sc] |
  let shsb = (x.sb) . (hb[x]) . (x.sb) |
  let eco = ^(x.rf + x.co + rb[x]) |
  let psc = Esc . (rc[Fsb[x]]) . ((x.sb) + shsb + eco) . (rc[sbF[x]]) . Esc |
  is_acyclic[psc]
}

// Restoring cumulativity of SC fences:
pred consistent3[x:Exec_C]{
  HbCom[x]
  NaRf[x]
  let Esc = stor[x.sc] |
  let shsb = (x.sb) . (hb[x]) . (x.sb) |
  let eco = ^(x.rf + x.co + rb[x]) |
  let Fhb = (stor[x.F]) . (hb[x]) |
  let hbF = (hb[x]) . (stor[x.F]) |
  let psc = Esc . (rc[Fhb]) . ((x.sb) + shsb + eco) . (rc[hbF]) . Esc |
  is_acyclic[psc]
}

// Allowing elimination of SC accesses:
pred consistent4[x:Exec_C]{
  HbCom[x]
  NaRf[x]
  let Esc = stor[x.sc] |
  let shsb = (x.sb - x.sloc) . (hb[x]) . (x.sb - x.sloc) |
  let eco = ^(x.rf + x.co + rb[x]) |
  let Fhb = (stor[x.F]) . (hb[x]) |
  let hbF = (hb[x]) . (stor[x.F]) |
  let psc = Esc . (rc[Fhb]) . ((x.sb) + shsb + eco) . (rc[hbF]) . Esc |
  is_acyclic[psc]
}

// Allowing further elimination of SC accesses:
pred consistent5[x:Exec_C]{
  HbCom[x]
  NaRf[x]
  let Esc = stor[x.sc] |
  let shsb = (x.sb - (x.sloc & (x.W -> x.W))) . (hb[x]) .
             (x.sb - (x.sloc & (x.R -> x.R))) |
  let eco = ^(x.rf + x.co + rb[x]) |
  let Fhb = (stor[x.F]) . (hb[x]) |
  let hbF = (hb[x]) . (stor[x.F]) |
  let psc = Esc . (rc[Fhb]) . ((x.sb) + shsb + eco) . (rc[hbF]) . Esc |
  is_acyclic[psc]
}
