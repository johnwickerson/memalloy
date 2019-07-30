module sql_standard_no_sz[E]

open sql_standard_base[E]

pred consistent[e:PTag->E, X:Exec_SQL] { base_consistent[e, X] }
pred dead[e:PTag->E, X:Exec_SQL] { base_dead[e, X] }
