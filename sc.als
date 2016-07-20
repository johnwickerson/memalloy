module sc[E]
open exec[E]

pred consistent[x:Exec] {
  is_acyclic[x.sb + x.rf + fr[x] + x.co]
}
