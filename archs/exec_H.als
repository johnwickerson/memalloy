module exec_H[E]
open exec[E]

sig Exec_H extends Exec {
}{
  // control dependencies are defined differently in assembly
  cd.sb in cd
}
