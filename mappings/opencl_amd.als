open ../archs/exec_OpenCL[E] as SW
open ../archs/amd_gpu as HW
module opencl_amd[E]

pred apply_map[X : SW/Exec_OpenCL, map : E -> HW/Inst] {

  X.ev = E

  map in (E one -> HW/Inst)

  // RMWs are not considered
  //no (X.(R&W))

  // Everything is a read or a write (fences are not considered)
  no (X.(ev - (R + W)))

  // SC atomics are not considered
  no (X.sc)

  // Relaxed atomics are not considered
  no (X.(A - (acq + rel)))

  // Non-atomic and workgroup-scoped load compiles to plain "Load"
  all e : X.((R - W) - dv) | one e.map && e.map in Load

  // Device-scoped, non-remote load compiles to "Inval ; Load" (buggy)
  all e : X.((R - W) & dv - rem) | 
    some disj i1,i2 : Inst | e.map = i1 + i2 and
      sb_imm[i1,i2] and
      i1 in Inval and i2 in Load 

  // Device-scoped, remote load not considered
  no X.((R - W) & dv & rem) 

  // Non-atomic and workgroup-scoped store compiles to plain "Store"
  all e : X.((W - R) - dv) | one e.map && e.map in Store
 
  // Device-scoped, non-remote store compiles to "Flush ; Store"
  all e : X.((W - R) & rel & dv - rem) |  some disj i1,i2 : Inst { 
    e.map = i1 + i2
    sb_imm[i1,i2]
    i1 in Flush
	i2 in Store 
  }

  // Device-scoped, remote store compiles to 
  // "Lock x; Flush; Store x; RemInval; Unlock x"
  all e : X.((W - R) & rel & dv & rem) | 
  some disj i1,i2,i3,i4,i5 : Inst {
    e.map = i1 + i2 + i3 + i4 + i5
    sb_imm[i1,i2] and sb_imm[i2,i3] and sb_imm[i3,i4] and sb_imm[i4,i5]
    i1 in Lock and i1.loc = i3.loc
    i2 in Flush
    i3 in Store
    i4 in RemInval
    i5 in Unlock and i5.loc = i3.loc
  }

  // Workgroup-scoped RMW compiles to "IncL1"
  all e : X.((R & W) - dv) | one e.map && e.map in IncL1

  // RMWs with wider scope not considered
  no X.((R & W) & dv)

  // All stores are non-zero 
  all i : Inst | i.wval != Zero
    
  // Stores to the same location write different values
  all disj i1, i2 : Inst |
    (i1 + i2 in Store + IncL1 and i1.loc = i2.loc) implies
	  i1.wval != i2.wval

  // the mapping preserves threads
  (X.sthd).map = map.(Inst <: same_thd :> Inst)

  // the mapping preserves work-groups
  (X.swg).map = map.(Inst <: same_wg :> Inst)

  // the mapping preserves sb
  (*(X.sb)).map = map.(Inst <: (same_thd & (then + iden)) :> Inst)

  // the mapping preserves loc in both directions
  (X.sloc) = map.({i1, i2 : Inst | same_loc[i1,i2]}).~map
  
  // the mapping preserves rf 
  all r : X.R | 
    no (r.~(X.rf)) => (
	  some ri : r.map |
      ri.rval = Zero
    ) else (
      some wi : (r.~(X.rf)).map | some ri : r.map |
	  some ri.rval and ri.rval = wi.wval
    )

  // the mapping preserves co in both directions
  (X.co).map = map.derived_co

}


