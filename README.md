
| Licence | Master branch | Dev branch |
|---------|---------------|------------|
| [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) | [![Build Status](https://travis-ci.org/johnwickerson/memalloy.svg?branch=master)](https://travis-ci.org/johnwickerson/memalloy) | [![Build Status](https://travis-ci.org/johnwickerson/memalloy.svg?branch=dev)](https://travis-ci.org/johnwickerson/memalloy) |

# System requirements

- OCaml 4.07.0 or later (tested with 4.07.0)

- OPAM packages `xml-light`, `ocamlfind`, and `ocamlbuild`

- Python 2.7

- Python packages `progressbar` and `subprocess32`

- Java runtime version 8 (Java SE 8).

- Graphviz

- Apache Ant (for building Alloy)

# Quick start

1. Modify `configure.sh` to suit your OS.

2. Run `source configure.sh`.

3. Run `make install`. HTML documentation can now be browsed at
   `doc/index.html`.

4. Run `make quicktest`. After a few minutes you should find some
   pictures of distinguishing executions in the `png` directory.

# Converting .cat models to Alloy (.als) format

- Each `.cat` file must begin with a description of the architecture
  being modelled. This must be one of: `"BASIC"`, `"C"`, `"HW"`,
  `"X86"`, `"PPC"`, `"ARM7"`, `"ARM8"`, `"PTX"`, `"OpenCL"`, or `"OCaml"`.

- A reasonable fragment of the `.cat` language is supported.

	- You can define sets and relations via `let x = e`. Names of sets
      must begin with an uppercase letter, and names of relations must
      begin with a lowercase letter.

	- You can define functions via `let f(r1,...,rn) = e`. The name of
      the function must begin with an uppercase letter if the function
      returns a set, and must begin with a lowercase letter if the
      function returns a relation. Functions cannot return functions.
      Set-valued parameters must have a name beginning with an
      uppercase letter, and relation-valued parameters must begin with
      a lowercase letter. Parameters cannot be functions themselves.
	  
    - You can define relations (but not sets) recursively via `let x1
	  = e1 and ... and xn = en`, and these are unrolled a fixed number
	  of times when translating into Alloy (since Alloy only checks up
	  to a finite bound anyway). The number of unrollings is set by
	  the `-u` flag, which defaults to 3.
	  
    - You can define a consistency axiom of the model called `name`
      via `acyclic|irreflexive|empty e as name`. You can define a
      'definedness' axiom (i.e., one that must hold of every
      consistent execution or else the whole program is undefined) by
      prepending the statement above with `undefined_unless`, and you
      can define a 'deadness' axiom (i.e., one that must hold of an
      inconsistent execution in order to guarantee that the resultant
      litmus test has no other passing executions) by prepending the
      statement above with `deadness_requires` instead.

    - You can include the definitions and axioms of the `submodel.cat`
      file via `include submodel.cat`. 

- There are a few syntactic restrictions on `.cat` files.

    - The variable `int`, built into Herd, clashes with a keyword in
      Alloy, so is not allowed. You can use `thd` instead.

    - The variable `X`, built into Herd, clashes with another variable
      in Alloy, so is not allowed. You can use `domain(atom) |
      range(atom)` instead.

    - The variables `L` and `A` are used in Herd for 'release' and
      'acquire' accesses in the Arm8 architecture, but these clash
      with the variables for 'local' accesses in OpenCL and 'atomic'
      accesses in C and OpenCL, respectively. Alloy does not allow
      variables to be re-used in this way, so you must use `SCREL` and
      `SCACQ` in the Arm8 architecture instead.
