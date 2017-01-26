module sc[E]
open basic[E]
open ../archs/exec[E]

pred consistent[e:E, x:Exec] {
  is_acyclic[sb[e,x] + rf[e,x] + fr[e,x] + co[e,x]]
}
