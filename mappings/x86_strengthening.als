open ../archs/exec_x86[E]

open strengthening[E]

/*
A x86-to-x86 mapping that allows:
- sb edges to be introduced,
- threads to be interleaved,
- dependencies to be introduced,
- non-locked operations to become locked
*/

module x86_strengthening[E]

pred apply_map_x86[X,X':Exec_X86] { 

  apply_map[X,X']

  // the strengthened execution may contain *more* locked events
  X.LOCKED in X'.LOCKED

  // atomicity is unchanged
  X.atom = X'.atom

  // sb is unchanged
  X.sb = X'.sb
}
