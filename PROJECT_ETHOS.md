# Project Ethos

AspectGameEngine should be a small, reliable engine layer for tile maps, editor maps, layered entities, visibility, occlusion, serialization, localization, and game-facing helpers.

The core rules:

Ensure that optimization and performance are top of mind, separate editor from runtime and ensure editor state is immutable. The game handles state and transformations and rendering is handled at the godot game layer. 

Build robust foundations, not throwaway demos. Prototype-sized slices are fine when they harden the base system, but avoid teardown demo code that would need to be replaced before real gameplay can build on it.

Documentation must take the form of a live, changing section with prose--a friendly description of the algorithm or task of focus, a section covering limitations, and below that, a historical section with appended dated entries.
