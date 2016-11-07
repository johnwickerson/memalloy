# Automatically Comparing Memory Consistency Models

This repository contains materials for recreating and building upon the results
published in the following paper:

* [John Wickerson](http://johnwickerson.github.io),
  [Mark Batty](https://www.cs.kent.ac.uk/people/staff/mjb211/),
  [Tyler Sorensen](https://www.doc.ic.ac.uk/~tsorensen/), and
  [George A. Constantinides](http://cas.ee.ic.ac.uk/people/gac1/),
  "Automatically Comparing Memory Consistency Models", in
  Proc. *Principles of Programming Languages (POPL)*, 2017. To
  appear. [[Preprint]](http://johnwickerson.github.io/papers/memalloy.pdf).

The main results are summarised in Table 2 on
[page 11 of the paper](http://johnwickerson.github.io/papers/memalloy.pdf#page=11). The
primary aim of this page is to provide instructions for reproducing
the results in that table.

## Installation

Most of our models rely only on
[the basic Alloy tool](http://alloy.csail.mit.edu/alloy/), but some
depend on the higher-order quantification that is only supported in
[the AlloyStar tool](http://alloy.mit.edu/alloy/hola/). An unofficial
copy of AlloyStar (incorporating a couple of minor tweaks) can be
downloaded from
[our GitHub repository](https://github.com/johnwickerson/AlloyStar).

The files in this repository can be downloaded and then opened individually
in Alloy. This repository is structured into four subdirectories:

* `hw`: architecture-level memory consistency models

* `sw`: language-level memory consistency models

* `mappings`: language-to-architecture compiler mappings

* `tests`: questions about memory consistency models and the
relationships between them

## Running Alloy

* Set the solver to *Glucose* or *Plingeling* (via `Options →
  Solver`). Set the maximum memory usage and stack size as high as possible,
  e.g. 4GB of memory and 64MB of stack (via `Options → Maximum
  memory` and `Options → Maximum stack`). Set the maximum number of
  CEGIS iterations to 0, which indicates 'no maximum' (via `Options →
  Max CEGIS iterations`).

* Open an Alloy model file, e.g. `tests/Q2_c11_simp_orig.als` (via
`File → Open...`).

* Run a command by picking it from the `Execute` menu (e.g. `Execute →
Run gp for exactly 1 Exec, 5 E expect 1`).

* Alloy will respond with either "No instance found" or "Instance
  found". In the latter case, click on "Instance" to load the Alloy
  Visualizer.
  
* When opening an instance in the Alloy Visualizer, change the
  `Projection` setting from `none` to `Exec`. This greatly improves
  readability. If the instance involves two executions, click on the
  `<<` and `>>` buttons at the bottom to switch between them. If the
  instance involves separate hardware and software executions, project
  over both types of execution at the same time for maximum
  readability.

* In the Alloy Visualizer, the `Theme` button allows nodes and edges
to be hidden or restyled according to their type.


## Reproducing Results

In the table below, the **Task** column refers to the row in Table 2 on
[page 11 of the paper](http://johnwickerson.github.io/papers/memalloy.pdf#page=11).
The **File** column refers to a file in this repository, and
the **Command** column gives the command in that file that should be
executed (by selecting it from the `Execute` menu). The **Solver** column identifies the SAT solver that was
found to provide the fastest result. The **Time** column gives the
number of seconds to encode (first number) and to solve (second
number) the task. These numbers were obtained on a 64-bit Linux machine with four 16-core 2.1 GHz AMD
Opteron processors and 128 GB of RAM; results obtained on different
machines may vary considerably given the highly unpredictable nature
of SAT solving. Finally, the **Instance?** column reports whether an
instance is found or not.

| Task | File | Command | Solver | Time /s | Instance? |
|------|------|---------|--------|---------|-----------|
| 1 | `tests/Q2_c11_sra_simp.als` | `run gp for 1 Exec, 6 E, 3 Int expect 1` | *Glucose* | 0.7+0.6 | yes |
| 2 | `tests/Q2_c11_swrf_simp.als` | `run p1 for 1 Exec, 7 E, 4 Int expect 0` | *Glucose* | 0.8+625 | no |
| 3 | `tests/Q2_c11_swrf_simp.als` | `run p1 for 1 Exec, 12 E, 4 Int expect 1` | *Plingeling* | 2+214 | yes |
| 4 | `tests/Q2_c11_simp_orig.als` | `run gp for exactly 1 Exec, 5 E expect 1` | *Glucose* | 0.4+0.3 | yes |
| 5 | `tests/Q2_x86_mca.als` | `run gp for 1 Exec, 9 E, 5 Int expect 0` | *Plingeling* | 0.8+607 | no |
| 6 | `tests/Q2_ppc_mca.als` | `run gp for 1 Exec, 6 E, 5 Int expect 1` | *Glucose* | 1.5+0.06 | yes |
| 7 | `tests/Q2_sc_c11nodrf.als` | `run gp for 1 Exec, 4 E` | *Glucose* | 0.4+0.04 | yes |
| 8 | `tests/Q2_ptx.als` | `run gp for exactly 1 Exec, 7 E expect 1` | *Glucose* | 0.7+4 | yes |
| 9 | `tests/Q3_c11_seq.als` | `run gp for 2 Exec, 5 E expect 0` | *MiniSat* | 0.5+163 | no |
| 10 | `tests/Q3_c11_seq.als` | `run gp for 2 Exec, 6 E expect 1` | *Plingeling* | 0.7+5 | yes |
| 11 | `tests/Q3_c11_mo.als` | `run gp for 2 Exec, 7 E, 3 Int expect 1` | *Glucose* | 0.9+51 | yes |
| 12 | `tests/Q4_c11_x86a.als` | `run gp for exactly 1 c11_x86a/SW/exec/Exec, exactly 1 c11_x86a/HW/exec_H/exec/Exec, 6 HE, 6 SE` | *Plingeling* | 0.7+13029 | no |
| 13 | `tests/Q4_c11_ppc_trimmed.als` | `run gp for exactly 1 c11_ppc_trimmed/SW/exec/Exec, exactly 1 c11_ppc_trimmed/HW/exec_H/exec/Exec, 8 HE, 6 SE` | *Plingeling* | 8+91 | yes |
| 14 | `tests/Q4_opencl_amd.als` | `run p2 for exactly 1 M1/Exec, 2 E, 4 GState, 6 LState, 6 MemEntry, 3 Val, 1 Loc, 9 Action expect 1` | *Glucose* | 6+1355 | yes |
| 15 | `tests/Q4_opencl_amd.als` | `run p1 for exactly 1 M1/Exec, 4 E, 3 GState, 7 LState, 5 MemEntry, 2 Val, 2 Loc, 10 Action expect 1` | *Plingeling* | 16+4743 | yes |
| 16 | `tests/Q4_opencl_ptx_orig.als` | `run gp for exactly 1 M2/exec_ptx/exec_H/exec/Exec, exactly 1 opencl_ptx/SW/exec_C/exec/Exec, 8 HE, 5 SE, 4 Int expect 1` | *Plingeling* | 2+11 | yes |
| 17 | `tests/Q4_opencl_ptx_cumul.als` | `run gp for exactly 1 M2/exec_ptx/exec_H/exec/Exec, exactly 1 opencl_ptx1/opencl_ptx_base/SW/exec_C/exec/Exec, 15 HE, 5 SE, 4 Int` | *Plingeling* | 4+9719 | no |

## Going Further

* For learning more about the Alloy language, the definitive reference
  is [Daniel Jackson's book](http://softwareabstractions.org/). There
  is also a good
  [online tutorial](http://alloy.mit.edu/alloy/tutorials/online/).
