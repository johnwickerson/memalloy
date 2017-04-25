module relations[E]
open util/relation

pred is_empty[r : E -> E] {
  no r
}

pred is_acyclic[r : E -> E] {
  acyclic[r, E]
}

// reflexive closure (the ?-operator in Herd)
fun rc[r : E -> E] : E -> E {
  r + (E <: iden)
}

// lift a set to a relation (the [_] operator in Herd)
fun stor[s : set E] : E -> E {
  s <: iden
}

pred is_equivalence[r : E -> E, s : set E] {
  equivalence[r,s]
  r in s->s
}

pred strict_partial_order[r:E->E] {
  is_acyclic[r]
  transitive[r]
}

fun sq[s : set E] : E -> E {
  s -> s
}

fun imm[r : E -> E] : E -> E{
  r - (r.^r)
}
