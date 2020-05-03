# bigball

[![Build Status][status-png]][status]

Display the dependencies of a Microsoft Visual Studio solution file.

The solution file from Microsoft Visual Studio contains a dependency graph of
its projects. This program parses it and writes HTML files to visualize the
dependencies. It uses [vis][vis] to embed graphs in HTML files. 

The output files contains an index.html file showing:
- a link to visualize the full dependency graph
- the list of the projects and for each of them: a link to the direct dependency
  graph and a linke to the full dependency graph

The project page also displays the list of the reverse dependencies.

# Build and install

```
stack install
```

# Run

This will parse `myproject.sln` and write the HTML files into the `output`
directory.
```
bigball --input myproject.sln --output output
```


  [status]: https://travis-ci.org/jecaro/bigball?branch=master
  [status-png]: https://travis-ci.org/jecaro/bigball.svg?branch=master
  [vis]: https://visjs.org/

