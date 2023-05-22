# bigball

[![CI][status-png]][status]

Display the dependencies of a Microsoft Visual Studio solution file.

The solution file from Microsoft Visual Studio contains a dependency graph of
its projects. This program parses it and writes HTML files to visualize the
dependencies. It uses [vis][vis] to embed graphs in the HTML files.

The output files contain an index.html file showing:
- a link to visualize the full dependency graph
- the list of the projects and for each of them: a link to the direct dependency
  graph and a link to the full dependency graph

The project page also displays the list of the reverse dependencies.

# Build and install

```
nix profile install
```

# Run

This will parse `myproject.sln` and write the HTML files into the `output`
directory.
```
bigball --input myproject.sln --output output
```

# Example

Below are some screenshots of the pages generated with [OpenSceneGraph][osg]
solution file:

<img src="https://raw.githubusercontent.com/jecaro/bigball/master/docs/osgPresentation.png" width="300">
<img src="https://raw.githubusercontent.com/jecaro/bigball/master/docs/osgUI.png" width="300">

and the long [index page][osgindex] with its 268 projects.


  [status]: https://github.com/jecaro/bigball/actions
  [status-png]: https://github.com/jecaro/bigball/workflows/CI/badge.svg
  [vis]: https://visjs.org/
  [osgindex]: docs/osgindex.png
  [osg]: http://www.openscenegraph.org/

