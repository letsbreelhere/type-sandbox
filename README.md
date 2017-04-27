# Type System Sandboxes

This repo is a collection of implementations of various type systems on
the [lambda cube](https://en.wikipedia.org/wiki/Lambda_cube), intended
mostly as an exercise but also in order to have a canonical reference
interpretation of how various expressions should typecheck for language
development elsewhere. Most typing rules are drawn from the definitions
given in [Types and Programming Languages](https://www.amazon.com/Types-Programming-Languages-MIT-Press);
my (maybe too-ambitious) goal is to work up to the calculus of
constructions, as well as playing with subtyping, inference,
substructural type systems, etc. `F_ω` is currently the most
fleshed-out and therefore the "prettiest", though plenty of it is just
hacked together.
