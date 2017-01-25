/* Models from paper "Repairing SC in C/C++11 */

module c11_repairing[E]
open c11_base[E]

fun rb[e:E, x:Exec_C] : E->E {
  (stor[R[e,x]]) . ~(rf[e,x]) . (co[e,x])
}

// Batty et al:
pred consistent_batty[e:E, x:Exec_C]{
  HbCom[e,x]
  NaRf[e,x]
  let Esc = stor[sc[e,x]] |
  let psc = Esc . (rc[Fsb[e,x]]) . ((hb[e,x]) + co[e,x] + rb[e,x]) . (rc[sbF[e,x]]) . Esc |
  is_acyclic[psc]
}

// Repairing C11/Power mapping:
pred consistent1[e:E, x:Exec_C]{
  HbCom[e,x]
  NaRf[e,x]
  let Esc = stor[sc[e,x]] |
  let shsb = (sb[e,x]) . (hb[e,x]) . (sb[e,x]) |
  let psc = Esc . (rc[Fsb[e,x]]) .((sb[e,x]) + shsb + rf[e,x] + co[e,x] + rb[e,x]) . (rc[sbF[e,x]]) . Esc |
  is_acyclic[psc]
}

// Stronger SC fences:
pred consistent2[x:Exec_C]{
  HbCom[e,x]
  NaRf[e,x]
  let Esc = stor[sc[e,x]] |
  let shsb = (sb[e,x]) . (hb[e,x]) . (sb[e,x]) |
  let eco = ^(rf[e,x] + co[e,x] + rb[e,x]) |
  let psc = Esc . (rc[Fsb[e,x]]) . ((sb[e,x]) + shsb + eco) . (rc[sbF[e,x]]) . Esc |
  is_acyclic[psc]
}

// Restoring cumulativity of SC fences:
pred consistent3[e:E, x:Exec_C]{
  HbCom[e,x]
  NaRf[e,x]
  let Esc = stor[sc[e,x]] |
  let shsb = (sb[e,x]) . (hb[e,x]) . (sb[e,x]) |
  let eco = ^(rf[e,x] + co[e,x] + rb[e,x]) |
  let Fhb = (stor[F[e,x]]) . (hb[e,x]) |
  let hbF = (hb[e,x]) . (stor[F[e,x]]) |
  let psc =
    Esc . (rc[Fhb]) . ((sb[e,x]) + shsb + eco) . (rc[hbF]) . Esc |
  is_acyclic[psc]
}

// Allowing elimination of SC accesses:
pred consistent4[e:E, x:Exec_C]{
  HbCom[e,x]
  NaRf[e,x]
  let Esc = stor[sc[e,x]] |
  let shsb = (sb[e,x] - sloc[e,x]) . (hb[e,x]) . (sb[e,x] - sloc[e,x]) |
  let eco = ^(rf[e,x] + co[e,x] + rb[e,x]) |
  let Fhb = (stor[F[e,x]]) . (hb[e,x]) |
  let hbF = (hb[e,x]) . (stor[F[e,x]]) |
  let psc =
    Esc . (rc[Fhb]) . ((sb[e,x]) + shsb + eco) . (rc[hbF]) . Esc |
  is_acyclic[psc]
}

// Allowing further elimination of SC accesses:
pred consistent5[e:E, x:Exec_C]{
  HbCom[e,x]
  NaRf[e,x]
  let Esc = stor[sc[e,x]] |
  let shsb = (sb[e,x] - (sloc[e,x] & (W[e,x] -> W[e,x]))) . (hb[e,x]) .
             (sb[e,x] - (sloc[e,x] & (R[e,x] -> R[e,x]))) |
  let eco = ^(rf[e,x] + co[e,x] + rb[e,x]) |
  let Fhb = (stor[F[e,x]]) . (hb[e,x]) |
  let hbF = (hb[e,x]) . (stor[F[e,x]]) |
  let psc =
    Esc . (rc[Fhb]) . ((sb[e,x]) + shsb + eco) . (rc[hbF]) . Esc |
  is_acyclic[psc]
}
