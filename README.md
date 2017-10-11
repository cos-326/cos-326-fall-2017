# OCaml - COS 326, Fall 2017

## Schedule

The class is broken up into 7 weeks. Each week has an assignment, lecture slides, some reading, and sometimes optional related material from the [Real World OCaml](https://realworldocaml.org/) book.

Note that not all assignments and notes have been posted as of the start of the course. We'll have to add them in as we go.

Week | Assignment | Lecture | Reading | RWO Chapter |
---- | ---------- | ------- | ------- | ----------- |
1    | [Easy as Pi][1] | [OCaml Intro][lec1.0], [Type Checking Basics][lec1.1], [Let, Tuples, Unit][lec1.2], [Induction, Lists and Nats][lec1.3] | [Functional Basics][8], [Type Checking][9], [Type-directed programming][10], [Thinking Inductively][11] | [1][rwo1], [2][rwo2], [3][rwo3]
2    | [Boxoffice Trivia][2] | [Inductive programming with the Naturals; Poly-HO][lec2.0], [More Data][lec2.1] | [Polymorphism and Higher-order Programming][12] | [6][rwo6] |
3    | [Map and Caml-Mathica][3] | [Datatype Design][lec3.1], [Implementing OCaml in OCaml][lec3.2], [OCaml Interpreters, Part 2][lec3.3] | [Equational Reasoning][13] | [7][rwo7] |
4    | [Interpreter and Program Correctness][4] |  | [Equational Reasoning about Natural Numbers and Trees][14] |  |
5    | [Moogle][5] |  |  | [4][rwo4], [8][rwo8], [9][rwo9] |
6    | [Lazy Programming][6] |  | [Using threads and futures][15], [Parallel Complexity Models][16], [Parallel Scheduling][17], [Parallel Sequences][18] |  |
7    | [Data-Parallel Programming][7] |  |  |  |

[1]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/ass/a1.php
[2]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/ass/a2.php
[3]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/ass/a3.php
[4]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/ass/a4.php
[5]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/ass/a5.php
[6]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/ass/a6.php
[7]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/ass/a7.php

[lec1.0]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/01-intro.pdf
[lec1.1]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/02a-simple-type-checking.pdf
[lec1.2]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/02-let-tuples.pdf
[lec1.3]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/03-inductive-thinking.pdf
[lec2.0]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/04-poly-ho.pdf
[lec2.1]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/05-more-data.pdf
[lec3.1]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/06-data-design.pdf
[lec3.3]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/06a-ocaml-interpreter.pdf
[lec3.3]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/lec/07-ocaml-interpreter2.pdf

[rwo1]: https://realworldocaml.org/v1/en/html/a-guided-tour.html
[rwo2]: https://realworldocaml.org/v1/en/html/variables-and-functions.html
[rwo3]: https://realworldocaml.org/v1/en/html/lists-and-patterns.html
[rwo6]: https://realworldocaml.org/v1/en/html/variants.html
[rwo7]: https://realworldocaml.org/v1/en/html/error-handling.html
[rwo8]: https://realworldocaml.org/v1/en/html/imperative-programming-1.html
[rwo4]: https://realworldocaml.org/v1/en/html/files-modules-and-programs.html
[rwo9]: https://realworldocaml.org/v1/en/html/functors.html

[8]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/basics.php
[9]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/type-check.php
[10]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/intro.php
[11]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/recursion.php
[12]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/polymorphism.php
[13]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/reasoning.php
[14]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/reasoning-data.php
[15]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/parallel.php
[16]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/parallel-complexity.php
[17]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/parallel-schedules.php
[18]: http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/parallel-sequences.php

## More Links

- [Assignments](http://www.cs.princeton.edu/courses/archive/fall17/cos326/assignments.php)
- [Full Schedule (includes option reading)](http://www.cs.princeton.edu/courses/archive/fall17/cos326/schedule.php)
- [Notes](http://www.cs.princeton.edu/courses/archive/fall17/cos326/notes/index.php)
- [Textboox](https://realworldocaml.org/)

## Getting Started

### Installing OCaml

Install OCaml & the package manager

```bash
brew install ocaml opam
```

Install Merlin for editor support, and OUnit for testing

```bash
opam install merlin
opam install ounit
```

_If there's a build failure, try restarting your terminal._

For full install instructions see [here](http://www.cs.princeton.edu/courses/archive/fall17/cos326/resources.php).

### Clone the Repo (Don't Fork!!)

```bash
git clone git@github.com:cos-326/cos-326-fall-2017.git
```

### Create a Branch

Create a new branch off of `master` whose name is the same as your GitHub username. Then, push the branch to the remote (GitHub) repository. If you don't have permission to push to this repository, we'll add you to the [organization](https://github.com/cos-326/).

```bash
cd cos-326-fall-2017
git checkout -b laser
git push origin laser
```

## Running tests

To run tests for an assignment, you'll have to cd into the assignment directory, compile the tests, then run them.

```bash
cd a1
ocamlfind ocamlc -o test -package oUnit -linkpkg -g a1.ml test.ml
./test
```

## Docker

There is a minimal `Dockerfile`` and `docker-compose.yml` which creates an Ocaml/Opam development environment and
installs `utop`.

NOTE: this installs the latest maintained 'ocaml/opam' image, which as of now is version `4.04.2`.

Quickstart:
```
docker-compose build
```
This will download the base image and install `utop`.

```
./scripts/dtop
```

This will start `utop` interactively.

This script exists as a convenience and example (examine it to see how one might run a shell or the `ocaml` toplevel as
well).
