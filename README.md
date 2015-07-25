# rayhs
A raytracer written in Haskell

RayHS is a raytracer written in Haskell, this project's goal is maily to learn Haskell with a practical application but the goal is also to implement a full-featured phisicaly based renderer.
The current implementation is a simple raytracer with support for basic shapes and triangle meshe.

More details and screenshots can be found on the project page: http://www.orouiller.net/projects/ray-hs

Building the project requires a Haskell compiler such as GHC.
There are a few dependencies that can be installed via Cabal

cabal install --only-dependencies

The project should build with cabal build but I provided a script build.sh with some additional compilation flags (until this is done in the cabal cnofiguration...).

There is an aditional script test.sh that builds the program, runs it and opens the output image.
Currently only ppm file format is suported for output, these files can be opened by The Gimp for example.

Currently the scenes to be rendered are hardcoded in the source, at the end of rayTracer.hs.
