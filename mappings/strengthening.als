open ../archs/exec[E]
module strengthening[E]

pred apply_map[X, X' : Exec] { 

  X.ev = X'.ev
  
  // reads compile to reads
  X.R = X'.R

  // writes compile to writes
  X.W = X'.W

  // fences compile to fences
  X.F = X'.F
 
  // the mapping preserves sb, but only one
  // way (that is, it may introduce new sb
  // edges)  
  X.sb in X'.sb

  // the mapping preserves dependencies. It may
  // introduce new dependencies.
  X.cd in X'.cd
  X.ad in X'.ad
  X.dd in X'.dd
    
  // the mapping preserves rf in both directions
  X.rf = X'.rf

  // the mapping preserves co in both directions
  X.co = X'.co

  // the mapping preserves loc in both directions
  X.sloc = X'.sloc

  // the mapping preserves naL in both directions
  X.naL = X'.naL

  // the mapping preserves the thd-equivalence,
  // but only one way (that is, it may introduce
  // new thd-edges)
  X.sthd in X'.sthd
}
