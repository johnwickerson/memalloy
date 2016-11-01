/* Manually generated from opencl_scopedsc.cat */

module opencl_scoped[E]
open opencl_base[E]

pred Sscoped[x : Exec_OpenCL] {
  let scb = x.co + fr[x] + (hb[x]) |
  let FscbF = (rc[Fsb[x]]) . scb . (rc[sbF[x]]) |
  let scp = FscbF & (x.sc -> x.sc) - iden |
  is_acyclic[scp & incl[x]]
}		      		      

pred consistent[x : Exec_OpenCL] {
  Ghb[x]
  Lhb[x]
  Gcoh[x]
  Lcoh[x]
  Rf[x]
  GnaRf[x]
  LnaRf[x]
  Rmw[x]
  Sscoped[x]
}
