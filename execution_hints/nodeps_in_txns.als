pred nodeps_in_txns[X:Exec] {
  no (X.stxn & ((addr[none,X]) + (ctrl[none,X]) + (data[none,X])))
}
