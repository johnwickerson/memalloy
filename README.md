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

Memory consistency modelling using Alloy.

Alloy models can be executed in
[AlloyStar](http://alloy.mit.edu/alloy/hola/). An unofficial copy of
AlloyStar, incorporating a couple of minor tweaks, can be downloaded
from
[a GitHub repository](https://github.com/johnwickerson/AlloyStar). Some
tips:

* Set the solver to *Glucose* or *Plingeling*, set the maximum memory
  usage and stack size as high as possible (e.g. 4GB of memory and
  64MB of stack), and set the maximum number of CEGIS iterations to 0
  (which indicates 'no maximum').
  
* When opening a solution in the Alloy Visualizer, change the
  *Projection* setting from *None* to *Exec*. This makes solutions much more
  readable.

## Directory structure

* **hw**: architecture-level memory consistency models

* **sw**: language-level memory consistency models

* **mappings**: language-to-architecture compiler mappings

* **tests**: questions about memory consistency models and the
relationships between them
