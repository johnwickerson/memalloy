[![Build Status](https://travis-ci.org/johnwickerson/memalloy.svg?branch=master)](https://travis-ci.org/johnwickerson/memalloy)

# System requirements

- OCaml version >= 4.01.0

# Quick start

1. Modify `configure.sh` to suit your OS.

2. Run `source configure.sh`.

3. Run `make`. HTML documentation can now be browsed at `doc/index.html`.

4. Run `make quicktest`. After a few minutes you should find some
   pictures of distinguishing executions in the `png` directory.

# Converting .cat models to Alloy (.als) format

- To convert `models_cat/model.cat` into `models_als/model.als`,
  change to the `models_cat` directory and then run the command
  `../comparator/cat2als model.cat`.

- Each `.cat` file must begin with a description of the architecture
  being modelled. This must be one of: `"BASIC"`, `"C"`, `"HW"`,
  `"X86"`, `"PPC"`, `"ARM7"`, `"ARM8"`, `"PTX"`, or `"OpenCL"`.

- A reasonable fragment of the `.cat` language is supported.

    - You can define sets or relations via `let x = e`.

    - You can define functions via `let f(r1,...,rn) = e`, but note
      that the parameters `r1` through `rn` are assumed to be
      relations.
	  
    - You can define relations recursively via `let x1 = e1 and
      ... and xn = en`, and these are unrolled a fixed number of times
      when translating into Alloy (since Alloy only checks up to a
      finite bound anyway). The number of unrollings is set by the
      `-u` flag, which defaults to 3.
	  
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
      file via `include submodel.cat`. You will need to run `cat2als`
      on `submodel.cat` separately. When running `cat2als` on a
      sub-model, it is not necessary to form the overall
      consistency/definedness/deadness predicates, so pass the `-i`
      flag (for 'intermediate model') to inhibit this.

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
      variables to be re-used in this way, so you must use `screl` and
      `scacq` in the Arm8 architecture instead.
