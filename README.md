Eventail
========

Toy project to format Ada source code into a set of HTML pages, with
cross-reference hyper-links.

This tool is based on project files (`*.gpr`) and requires the projects to be
built, so that cross-reference information is available in library files
(`*.ali`). It also needs `pygmentize`, a program that the
[Pygments](http://pygments.org/) Python package provides in order to do the
syntax highlighting.


Build
-----

Just get a recent enough version of GNAT, make GNATcoll available to it and
then run from this directory:

```sh
$ gprbuild -P eventail.gpr -p
```

Once completed, the above command will produce the `bin/eventail` binary, which
is a standalone program.


Usage
-----

Move to a directory where you want to produce the set of HTML pages and make
the set of project files you want to render available (updating the
`$GPR_PROJECT_FILE` environment variable, for instance). Then, run:

```sh
$ eventail project_A.gpr project_B.gpr project_C.gpr
```

This will create a tree of HTML and CSS files. You can then open the entry
point in a browser: `index.html`.
