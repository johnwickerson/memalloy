module exec_arm7[E]
open exec_H[E]

sig Exec_Arm7 extends Exec_H {}{}

pred wf_Exec_Arm7 [X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb,isb:E->E] {

  wf_Exec_H[X,ad,cd,dd]

  is_fence_rel[dmbst, X.sb]
  is_fence_rel[dmbld, X.sb]
  is_fence_rel[dmb, X.sb]
  is_fence_rel[isb, X.sb]

  dmb in dmbst
  dmb in dmbld 
}

fun isb[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb,isb:E->E] : E->E { isb - (univ -> e) - (e -> univ) }
fun dmbst[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb,isb:E->E] : E->E { dmbst - (univ -> e) - (e -> univ) }
fun dmbld[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb,isb:E->E] : E->E { dmbld - (univ -> e) - (e -> univ) }
fun dmb[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb,isb:E->E] : E->E { dmb - (univ -> e) - (e -> univ) }

// Synonyms:
fun ISB[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb,isb':E->E] : E->E { isb[e,X,ad,cd,dd,dmbst,dmbld,dmb,isb'] }
fun DMB[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb',isb:E->E] : E->E { dmb[e,X,ad,cd,dd,dmbst,dmbld,dmb',isb] }
fun DSB[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb',isb:E->E] : E->E { dmb[e,X,ad,cd,dd,dmbst,dmbld,dmb',isb] } // dsb = dmb
fun DMBSY[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld,dmb',isb:E->E] : E->E { dmb[e,X,ad,cd,dd,dmbst,dmbld,dmb',isb] }
fun DMBST[e:E, X:Exec_Arm7, ad,cd,dd,dmbst',dmbld,dmb,isb:E->E] : E->E { dmbst[e,X,ad,cd,dd,dmbst',dmbld,dmb,isb] }
fun DMBLD[e:E, X:Exec_Arm7, ad,cd,dd,dmbst,dmbld',dmb,isb:E->E] : E->E { dmbld[e,X,ad,cd,dd,dmbst,dmbld',dmb,isb] }
fun DSBST[e:E, X:Exec_Arm7, ad,cd,dd,dmbst',dmbld,dmb,isb:E->E] : E->E { dmbst[e,X,ad,cd,dd,dmbst',dmbld,dmb,isb] } // dsb = dmb
