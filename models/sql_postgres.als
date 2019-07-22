module sql_postgres[E]

open ../archs/exec_SQL[E]

fun hb[e:PTag->E, X:Exec_SQL] : E->E {
  let write_rf_to_rc = (rf[e, X] - sthd[e, X]) :> rc[e, X]
    , commit_before_rc_read = ~(commit_of[X]) . write_rf_to_rc
  | commit_before_rc_read
}

pred consistent[e:PTag->E, X:Exec_SQL] {
  is_acyclic[hb[e, X]]
}
