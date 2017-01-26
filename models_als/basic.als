/* Automatically generated from basic.cat on 2017-01-26 at 14:31:09 */

module basic[E]
open ../archs/exec[E]

fun poloc [e:E, X:Exec] : E->E {
  (po[e,X]) & (sloc[e,X])
}

fun com [e:E, X:Exec] : E->E {
  (rf[e,X]) + (fr[e,X]) + (co[e,X])
}

