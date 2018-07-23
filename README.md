# Vis

This is a PureScript library for creating variational data visualizations with a
specific focus on exploratory and comparative data analysis tasks.  

## Installing

First, you need a Purescript setup on your system, with Pulp in particular.
The code is only tested with `purs` version `0.11.7` and `pulp` version
`11.0.2`.  It also requires a patched version of the Purescript canvas library,
which is available [in this fork](https://github.com/karljs/purescript-canvas)
at least until there is a better story for anti-clockwise arcs in the official
library.

Once you have that installed you should be able to simply clone this repository,
open it in a terminal, and run the following (user whatever port you want):

    pulp repl -- --port=1234

Next open `localhost:1234` in a browser.  Once all loading has finished, go back
to the REPL and load the library itself with the following command:

    import Main

Now you should be able to render any visualization you like by passing them to
the `go` function.  For example, to draw one of the simple built-in examples
try:

    go v1
