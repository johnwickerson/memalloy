# Automatically Comparing Memory Consistency Models

This repository contains materials for recreating the results
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
aim of this page is to give instructions for reproducing the results
in that table.

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

* Open an Alloy model file, e.g. `tests/Q2_c11_simp/question.als` (via
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

| hello  | testing  | blah  | foo  | qux  |
|---|:---:|---:|---|---|
| 1  | 2  | 3  | 4  |  5 |

## Going Further

* For learning more about the Alloy language, the definitive reference
  is [Daniel Jackson's book](http://softwareabstractions.org/). There
  is also a good
  [online tutorial](http://alloy.mit.edu/alloy/tutorials/online/).
