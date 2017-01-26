/* Manually generated from opencl_scopedsc.cat */

module opencl_scoped[E]
open opencl_base[E]

pred Sscoped[e:E, x : Exec_OpenCL] {
  let scb = co[e,x] + fr[e,x] + (hb[e,x]) |
  let FscbF = (rc[Fsb[e,x]]) . scb . (rc[sbF[e,x]]) |
  let scp = FscbF & (sc[e,x] -> sc[e,x]) - iden |
  is_acyclic[scp & incl[e,x]]
}		      		      

pred consistent[e:E, x : Exec_OpenCL] {
  Ghb[e,x]
  Lhb[e,x]
  Gcoh[e,x]
  Lcoh[e,x]
  Rf[e,x]
  GnaRf[e,x]
  LnaRf[e,x]
  Rmw[e,x]
  Sscoped[e,x]
}
