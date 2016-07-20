# memalloy
Memory consistency modelling using Alloy

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
  
