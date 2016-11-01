/* Manually generated from x86tso.cat */

module x86tso[E]
open exec_x86[E]

pred GHB[x : Exec_X86] {
  // ppo  
  let po_ghb = (x.W -> x.W) & x.sb + (x.R -> x.(R+W)) & x.sb |

  // mfence
  let mfence = ((x.sb) & (x.ev -> x.F)) . (x.sb) |

  // implied barriers
  let poWR = (x.W -> x.R) & (x.sb) |
  let i1 = (x.(ev-locked) -> x.locked) & poWR |
  let i2 = (x.locked -> x.(ev-locked)) & poWR |
  let implied = i1 + i2 |
            
  let ghb = mfence + implied + po_ghb + rfe[x] + fr[x] + x.co |
  is_acyclic[ghb]
}

pred consistent[x : Exec_X86] {
  Uniproc[x]
  Atomic[x]
  GHB[x]
}

run {
  some x : Exec_X86 | storebuffering_H[x] && consistent[x] 
} for exactly 1 Exec, 4 E

run {
  some x : Exec_X86 | iriw_H1[x] && not (consistent[x])
} for exactly 1 Exec, 6 E

