/* Manually generated from opencl_scopedsc.cat */

module opencl[E]
open opencl_base[E]

pred Sscoped[x : Exec_OpenCL] {
  let scp = ((rc[Fsb[x]]) . (x.co + fr[x] + (hb[x])) .
	     (rc[sbF[x]])) & (x.sc -> x.sc) - iden |
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

pred racefree[x : Exec_OpenCL] {
  Dr[x]
  Bd[x]
}
